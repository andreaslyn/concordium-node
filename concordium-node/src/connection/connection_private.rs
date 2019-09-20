use failure::Fallible;
use mio::{net::TcpStream, Event};
use snow::Keypair;

use crate::{
    common::p2p_peer::PeerType,
    connection::{
        async_adapter::{HandshakeStreamSink, Readiness},
        fails, Connection, FrameSink, FrameStream, MessageSendingPriority,
    },
};
use concordium_common::hybrid_buf::HybridBuf;

use std::{
    net::Shutdown,
    pin::Pin,
    sync::{Arc, RwLock},
};

pub struct ConnectionPrivate {
    pub conn_ref:       Option<Pin<Arc<Connection>>>,
    pub socket:         TcpStream,
    pub message_sink:   FrameSink,
    pub message_stream: FrameStream,
}

impl ConnectionPrivate {
    pub fn conn(&self) -> &Connection {
        &self.conn_ref.as_ref().unwrap() // safe; always available
    }

    pub fn new(
        peer_type: PeerType,
        socket: TcpStream,
        key_pair: Keypair,
        is_initiator: bool,
        noise_params: snow::params::NoiseParams,
    ) -> Self {
        let handshaker = Arc::new(RwLock::new(HandshakeStreamSink::new(
            noise_params.clone(),
            key_pair,
            is_initiator,
        )));

        ConnectionPrivate {
            conn_ref: None,
            socket,
            message_sink: FrameSink::new(Arc::clone(&handshaker)),
            message_stream: FrameStream::new(peer_type, handshaker),
        }
    }

    pub fn shutdown(&mut self) -> Fallible<()> {
        map_io_error_to_fail!(self.socket.shutdown(Shutdown::Both))
    }

    pub fn read_from_stream(&mut self, ev: &Event) -> Fallible<()> {
        let ev_readiness = ev.readiness();

        // 1. Try to read messages from `socket`.
        if ev_readiness.is_readable() {
            loop {
                let read_result = self.message_stream.read(&mut self.socket);
                match read_result {
                    Ok(readiness) => match readiness {
                        Readiness::Ready(message) => {
                            self.conn().send_to_dump(&message, true);
                            if let Err(e) = self.conn().process_message(message) {
                                warn!(
                                    "Terminating connection {} due to {}",
                                    usize::from(ev.token()),
                                    e
                                );
                                self.conn().handler().remove_connection(self.conn().token);
                                return Ok(());
                            }
                        }
                        Readiness::NotReady => break,
                    },
                    Err(err) => {
                        let token_id = usize::from(self.conn().token);

                        if err.downcast_ref::<fails::UnwantedMessageError>().is_none()
                            && err.downcast_ref::<fails::MessageTooBigError>().is_none()
                        {
                            warn!(
                                "Protocol error, connection {} is dropped: {}",
                                token_id, err
                            );
                        } else {
                            error!("Message stream error on connection {}: {:?}", token_id, err);
                        }

                        // In this case, we have to drop this connection, so we can avoid
                        // writing any data.
                        self.conn().handler().remove_connection(self.conn().token);
                        return Ok(());
                    }
                }
            }
        }

        // 2. Write pending data into `socket`
        self.message_sink.flush(&mut self.socket)?;

        Ok(())
    }

    pub fn write_to_sink(
        &mut self,
        input: HybridBuf,
        priority: MessageSendingPriority,
    ) -> Fallible<Readiness<usize>> {
        self.message_sink.write(input, &mut self.socket, priority)
    }
}

impl Drop for ConnectionPrivate {
    fn drop(&mut self) {
        use crate::connection::fails::PeerTerminatedConnection;
        if let Err(e) = self.shutdown() {
            if e.downcast_ref::<PeerTerminatedConnection>().is_none() {
                error!("ConnectionPrivate couldn't be closed: {:?}", e);
            }
        }
    }
}
