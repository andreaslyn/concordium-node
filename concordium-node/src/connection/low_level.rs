use byteorder::{ByteOrder, NetworkEndian, WriteBytesExt};
use failure::{Error, Fallible};
use mio::tcp::TcpStream;
use snow::{Keypair, Session};

use super::{
    async_adapter::{
        partial_copy, DecryptStream, EncryptSink, FrameStream, HandshakeStreamSink, PayloadSize,
        Readiness, NOISE_MAX_MESSAGE_LEN, PRE_SHARED_KEY, PROLOGUE,
    },
    fails::{MessageTooBigError, StreamWouldBlock, UnwantedMessageError},
    Connection, DeduplicationQueues,
};
use crate::{
    common::p2p_peer::PeerType,
    network::{ProtocolMessageType, PROTOCOL_HEADER_LEN, PROTOCOL_MAX_MESSAGE_SIZE},
};
use concordium_common::hybrid_buf::HybridBuf;

use std::{
    convert::TryFrom,
    io::{ErrorKind, Read},
    pin::Pin,
    sync::Arc,
};

/// State of the *IKpsk2* handshake:
/// ```ignore
/// <- s
///  A -> e,es,s,ss
///  B -> e,ee,se,psk
/// ```
#[allow(non_camel_case_types)]
#[derive(PartialEq)]
pub enum HandshakeState {
    ResponderAwaitingRequest_S,
    InitiatorAwaiting_S,
    ResponderAwaiting_E_ES_S_SS,
    InitiatorAwaiting_E_EE_SE_PSK,
    Complete,
}

pub struct ConnectionLowLevel {
    pub conn_ref:    Option<Pin<Arc<Connection>>>,
    pub socket:      TcpStream,
    keypair:         Keypair,
    noise_session:   Option<Session>,
    input_buffer:    [u8; NOISE_MAX_MESSAGE_LEN],
    output_queue:    Vec<HybridBuf>,
    handshake_state: HandshakeState,
    handshaker:      HandshakeStreamSink,
    message_stream:  FrameStream,
    decryptor:       Option<DecryptStream>,
    encryptor:       Option<EncryptSink>,
}

impl ConnectionLowLevel {
    fn conn(&self) -> &Connection {
        &self.conn_ref.as_ref().unwrap() // safe; always available
    }

    pub fn new(
        socket: TcpStream,
        keypair: Keypair,
        is_initiator: bool,
        noise_params: snow::params::NoiseParams,
    ) -> Self {
        if let Err(e) = socket.set_linger(Some(std::time::Duration::from_secs(0))) {
            error!(
                "Can't set SOLINGER to 0 for socket {:?} due to {}",
                socket, e
            );
        }

        let handshake_state = if is_initiator {
            HandshakeState::InitiatorAwaiting_S
        } else {
            HandshakeState::ResponderAwaitingRequest_S
        };

        let handshaker = HandshakeStreamSink::new(is_initiator);

        let noise_session = if is_initiator {
            None
        } else {
            Some(
                snow::Builder::new(noise_params)
                    .prologue(PROLOGUE)
                    .psk(2, PRE_SHARED_KEY)
                    .local_private_key(&keypair.private)
                    .build_responder()
                    .unwrap(),
            )
        };

        ConnectionLowLevel {
            conn_ref: None,
            socket,
            keypair,
            noise_session,
            input_buffer: [0u8; NOISE_MAX_MESSAGE_LEN],
            output_queue: Vec::with_capacity(16),
            handshake_state,
            handshaker,
            message_stream: FrameStream::new(),
            decryptor: None,
            encryptor: None,
        }
    }

    // handshake

    fn on_initiator_get_s(&mut self, mut input: HybridBuf) -> Fallible<Readiness<()>> {
        // Received: <- s
        let remote_public_key_vw = input.remaining_bytes()?;
        trace!(
            "Initiator has received ({} bytes): <- s",
            remote_public_key_vw.len()
        );

        let mut session = snow::Builder::new(
            self.conn()
                .handler()
                .connection_handler
                .noise_params
                .clone(),
        )
        .prologue(PROLOGUE)
        .psk(2, PRE_SHARED_KEY)
        .local_private_key(&self.keypair.private)
        .remote_public_key(&remote_public_key_vw)
        .build_initiator()?;

        // Send: A -> e,es,s,ss
        let buf_len = session.write_message(&[], &mut self.input_buffer)?;
        self.handshaker
            .send_queue
            .push_back(create_frame(&self.input_buffer[..buf_len])?);
        trace!("Initiator sends ({} bytes): A -> e,es,s,ss", buf_len);

        self.handshake_state = HandshakeState::InitiatorAwaiting_E_EE_SE_PSK;
        self.noise_session = Some(session);

        Ok(Readiness::NotReady)
    }

    fn on_responder_get_request_s(&mut self) -> Fallible<Readiness<()>> {
        // Send the public key
        self.handshaker
            .send_queue
            .push_back(create_frame(&self.keypair.public)?);
        trace!(
            "Responder sends its public key ({} bytes): <- s",
            self.keypair.public.len()
        );

        // Next state
        self.handshake_state = HandshakeState::ResponderAwaiting_E_ES_S_SS;

        Ok(Readiness::NotReady)
    }

    fn on_responder_get_e_ee_s_ss(&mut self, mut input: HybridBuf) -> Fallible<Readiness<()>> {
        // Received: A -> e,es,s,ss
        if let Some(mut session) = self.noise_session.take() {
            let e_es_s_ss = input.remaining_bytes()?;
            session.read_message(&e_es_s_ss, &mut self.input_buffer)?;

            trace!(
                "Responder has received ({} bytes): A -> e,es,s,ss",
                e_es_s_ss.len()
            );

            // Send: B -> e,ee,se,psk
            let buf_len = session.write_message(&[], &mut self.input_buffer)?;
            self.handshaker
                .send_queue
                .push_back(create_frame(&self.input_buffer[..buf_len])?);
            trace!("Responder sends ({} bytes):B -> e,ee,se,psk", buf_len);

            // Transport session is ready
            self.handshake_state = HandshakeState::Complete;
            self.noise_session = Some(session.into_stateless_transport_mode()?);

            Ok(Readiness::Ready(()))
        } else {
            unreachable!("Noise Session (no transport) is none");
        }
    }

    fn on_initiator_get_e_ee_se_psk(&mut self, mut input: HybridBuf) -> Fallible<Readiness<()>> {
        // B -> e,ee,se,psk
        if let Some(mut session) = self.noise_session.take() {
            let e_ee_se_psk = input.remaining_bytes()?;
            trace!(
                "Initiator has received ({} bytes): B -> e,ee,se,psk",
                e_ee_se_psk.len()
            );

            session.read_message(&e_ee_se_psk, &mut self.input_buffer)?;

            self.handshake_state = HandshakeState::Complete;
            self.noise_session = Some(session.into_stateless_transport_mode()?);

            Ok(Readiness::Ready(()))
        } else {
            unreachable!("Noise Session (no transport) is none");
        }
    }

    /// It reads from `input`, and based on current internal state, interprets
    /// it.
    ///
    /// # Return
    ///
    /// As soon as handshake is done, it will return
    /// `Readiness::Ready(session)`, which contains a transport session,
    /// ready to encrypt/decrypt. Otherwise, it returns
    /// `Readiness::NotReady`.
    pub fn read_handshake_msg(&mut self, input: HybridBuf) -> Fallible<Readiness<()>> {
        match self.handshake_state {
            HandshakeState::ResponderAwaitingRequest_S => self.on_responder_get_request_s(),
            HandshakeState::ResponderAwaiting_E_ES_S_SS => self.on_responder_get_e_ee_s_ss(input),
            HandshakeState::InitiatorAwaiting_S => self.on_initiator_get_s(input),
            HandshakeState::InitiatorAwaiting_E_EE_SE_PSK => {
                self.on_initiator_get_e_ee_se_psk(input)
            }
            HandshakeState::Complete => unreachable!(),
        }
    }

    // input

    #[inline(always)]
    pub fn read_stream(&mut self, deduplication_queues: &mut DeduplicationQueues) -> Fallible<()> {
        loop {
            match self.read_from_socket() {
                Ok(readiness) => match readiness {
                    Readiness::Ready(message) => {
                        self.conn().send_to_dump(&message, true);
                        if let Err(e) = self.conn().process_message(message, deduplication_queues) {
                            bail!("Can't read the stream: {}", e);
                        }
                    }
                    Readiness::NotReady => return Ok(()),
                },
                Err(e) => bail!("Can't read the stream: {}", e),
            }
        }
    }

    #[inline(always)]
    fn read_from_socket(&mut self) -> Fallible<Readiness<HybridBuf>> {
        let payload_readiness = if self.message_stream.expected_size == 0 {
            self.read_expected_size()
        } else {
            self.read_payload()
        };

        match payload_readiness {
            Ok(Readiness::Ready(payload)) => self.forward(payload),
            Ok(Readiness::NotReady) => Ok(Readiness::NotReady),
            Err(err) => {
                if err.downcast_ref::<StreamWouldBlock>().is_some() {
                    Ok(Readiness::NotReady)
                } else {
                    Err(err)
                }
            }
        }
    }

    /// It forwards the payload based on the current state, into:
    ///     a) `Handshaker` stream, if handshake is not yet completed.
    ///     b) `Decryptor` stream, to decrypt payload.
    fn forward(&mut self, input: HybridBuf) -> Fallible<Readiness<HybridBuf>> {
        if let Some(ref mut decryptor) = self.decryptor {
            Ok(Readiness::Ready(
                decryptor.decrypt(self.noise_session.as_ref().unwrap(), input)?,
            ))
        } else {
            if let Readiness::Ready(()) = self.read_handshake_msg(input)? {
                // Create decryptor using session.
                self.decryptor = Some(DecryptStream::new());
            }
            Ok(Readiness::NotReady)
        }
    }

    /// It allows packet with empty payloads.
    #[inline]
    fn pending_bytes_to_know_expected_size(&self) -> usize {
        if self.message_stream.message.len() < std::mem::size_of::<PayloadSize>() {
            std::mem::size_of::<PayloadSize>() - self.message_stream.message.len()
        } else {
            0
        }
    }

    /// It only reads the first 4 bytes of the message to determine its size.
    ///
    /// This operation could not be completed in just one shot due to limits of
    /// `output` buffers.
    pub fn read_expected_size(&mut self) -> Fallible<Readiness<HybridBuf>> {
        // Extract only the bytes needed to know the size.
        let min_bytes = self.pending_bytes_to_know_expected_size();
        let read_bytes =
            map_io_error_to_fail!(self.socket.read(&mut self.input_buffer[..min_bytes]))?;

        self.message_stream
            .message
            .extend_from_slice(&self.input_buffer[..read_bytes]);

        // Load expected size
        if self.message_stream.message.len() >= std::mem::size_of::<PayloadSize>() {
            // Update target: ignore first 4 bytes on data.
            self.message_stream.expected_size = NetworkEndian::read_u32(
                &self.message_stream.message[..std::mem::size_of::<PayloadSize>()],
            );
            self.message_stream.message.clear();
            trace!(
                "Expected message size is {}",
                self.message_stream.expected_size
            );

            // Check protocol limits
            if self.message_stream.expected_size > PROTOCOL_MAX_MESSAGE_SIZE as u32 {
                let error = MessageTooBigError {
                    message_size:  self.message_stream.expected_size,
                    protocol_size: PROTOCOL_MAX_MESSAGE_SIZE as u32,
                };
                self.message_stream.clear_input();
                return Err(Error::from(error));
            }

            // Pre-allocate some space. We don't allocate the expected size in order to
            // reduce the memory foot-print during transmission and the impact
            // of invalid packages.
            let pre_alloc_size = std::cmp::min(
                NOISE_MAX_MESSAGE_LEN,
                self.message_stream.expected_size as usize,
            );
            self.message_stream.message.reserve(pre_alloc_size);

            // Read data next...
            self.read_payload()
        } else {
            // We need more data to determine the message size.
            Ok(Readiness::NotReady)
        }
    }

    /// Once we know the message expected size, we can start to receive data.
    pub fn read_payload(&mut self) -> Fallible<Readiness<HybridBuf>> {
        // Read no more than expected, directly into our buffer
        while self.read_intermediate()? >= NOISE_MAX_MESSAGE_LEN {
            // Validation only on encrypted channels.
            if self.decryptor.is_some() && !self.message_stream.is_validated {
                match self.validate() {
                    Readiness::Ready(true) => self.message_stream.is_validated = true,
                    Readiness::Ready(false) => {
                        self.message_stream.clear_input();
                        return Err(Error::from(UnwantedMessageError {
                            message: "Packet is not valid for this node".to_owned(),
                        }));
                    }
                    _ => {}
                }
            }
        }

        if self.message_stream.expected_size == self.message_stream.message.len() as u32 {
            // Ready message
            let new_data = std::mem::replace(
                &mut self.message_stream.message,
                Vec::with_capacity(std::mem::size_of::<PayloadSize>()),
            );
            self.message_stream.clear_input();

            Ok(Readiness::Ready(HybridBuf::try_from(new_data)?))
        } else {
            Ok(Readiness::NotReady)
        }
    }

    /// It invalidates any `Packet` or unknown message when we are in
    /// Bootstrapper mode.
    fn validate(&mut self) -> Readiness<bool> {
        if self.conn().handler().self_peer.peer_type() == PeerType::Bootstrapper {
            if self.message_stream.message.len() > PROTOCOL_HEADER_LEN {
                let protocol_type =
                    ProtocolMessageType::try_from(self.message_stream.message[PROTOCOL_HEADER_LEN])
                        .unwrap_or(ProtocolMessageType::Packet);
                match protocol_type {
                    ProtocolMessageType::Packet => Readiness::Ready(false),
                    _ => Readiness::Ready(true),
                }
            } else {
                // The message doesn't have enough data yet.
                Readiness::NotReady
            }
        } else {
            // Any message is valid for non-boostrapper node.
            Readiness::Ready(true)
        }
    }

    fn read_intermediate(&mut self) -> Fallible<usize> {
        let pending_bytes =
            self.message_stream.expected_size - self.message_stream.message.len() as u32;
        let max_buff = std::cmp::min(pending_bytes as usize, NOISE_MAX_MESSAGE_LEN);

        match self.socket.read(&mut self.input_buffer[..max_buff]) {
            Ok(read_bytes) => {
                trace!(
                    "Appended {} bytes to current message of {} bytes, and an expected size of {} \
                     bytes",
                    read_bytes,
                    self.message_stream.message.len(),
                    self.message_stream.expected_size
                );
                self.message_stream
                    .message
                    .extend_from_slice(&self.input_buffer[..read_bytes]);

                Ok(read_bytes)
            }
            Err(err) => match err.kind() {
                ErrorKind::WouldBlock => Ok(0),
                _ => Err(Error::from(err)),
            },
        }
    }

    // output

    /// If a channel is encrypted, `encryptor` will take care of it.
    /// Otherwise, `input` is enqueued until `encryptor` is ready
    /// (after the handshake is done).
    #[inline(always)]
    pub fn write_to_socket(&mut self, input: HybridBuf) -> Fallible<Readiness<usize>> {
        if let Some(ref mut encryptor) = self.encryptor {
            encryptor.write_without_flush(self.noise_session.as_ref().unwrap(), input)?;
            self.flush_encryptor()
        } else {
            self.output_queue.push(input);
            self.flush_socket()
        }
    }

    #[inline(always)]
    pub fn flush_socket(&mut self) -> Fallible<Readiness<usize>> {
        if self.encryptor.is_some() {
            self.flush_encryptor()
        } else {
            // Ensure that handshake `send` queue is empty before moving on
            if let Readiness::Ready(0) = self.flush_handshaker()? {
                if self.handshake_state == HandshakeState::Complete {
                    // Create an encryptor and enqueue pending messages.
                    let mut encryptor = EncryptSink::new();

                    for frame in self.output_queue.drain(..) {
                        encryptor
                            .write_without_flush(self.noise_session.as_ref().unwrap(), frame)?;
                    }

                    // Move to next phase: encryption.
                    self.encryptor = Some(encryptor);
                    self.flush_encryptor()
                } else {
                    Ok(Readiness::NotReady)
                }
            } else {
                Ok(Readiness::NotReady)
            }
        }
    }

    fn flush_handshaker(&mut self) -> Fallible<Readiness<usize>> {
        if let Some(mut data) = self.handshaker.send_queue.pop_front() {
            let written_bytes = partial_copy(&mut data, &mut self.socket)?;

            // Requeue data if it is not eof.
            if !data.is_eof()? {
                self.handshaker.send_queue.push_front(data);
                Ok(Readiness::NotReady)
            } else {
                Ok(Readiness::Ready(written_bytes))
            }
        } else {
            Ok(Readiness::Ready(0))
        }
    }

    #[inline(always)]
    fn flush_encryptor(&mut self) -> Fallible<Readiness<usize>> {
        if let Some(ref mut encryptor) = self.encryptor {
            if let Some(mut encrypted) = encryptor.messages.pop_front() {
                partial_copy(&mut encrypted, &mut self.socket)?;

                if encrypted.is_eof()? {
                    // The message has been fully processed.
                    Ok(Readiness::Ready(encrypted.len()? as usize))
                } else {
                    // Message is not completed... re-queue the rest of it.
                    encryptor.messages.push_front(encrypted);
                    Ok(Readiness::NotReady)
                }
            } else {
                // The message queue is empty.
                Ok(Readiness::Ready(0))
            }
        } else {
            unreachable!()
        }
    }
}

/// It prefixes `data` with its length, encoded as `u32` in `NetworkEndian`.
pub fn create_frame(data: &[u8]) -> Fallible<HybridBuf> {
    let mut frame = Vec::with_capacity(data.len() + std::mem::size_of::<PayloadSize>());
    frame.write_u32::<NetworkEndian>(data.len() as u32)?;
    frame.extend_from_slice(data);

    into_err!(HybridBuf::try_from(frame))
}
