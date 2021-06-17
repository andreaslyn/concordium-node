use crate::{
    consensus_ffi::helpers::PacketType,
    network::{NetworkMessage, NetworkPayload, WireProtocolVersion, WIRE_PROTOCOL_LEGACY_VERSION},
};
use anyhow::bail;
use crypto_common::Deserial;
use std::{convert::TryFrom, io::Cursor, sync::Arc};

/// Rewrite an outgoing message from the current wire protocol version to the
/// specified version. Currently, this only supports rewriting to version 0
/// (WIRE_PROTOCOL_LEGACY_VERSION). A failure indicates that the message cannot
/// be converted, and so should not be sent to the peer. A result of None
/// indicates that no conversion is necessary.
pub fn rewrite_outgoing(
    wire_version: WireProtocolVersion,
    message: &[u8],
) -> anyhow::Result<Option<Arc<[u8]>>> {
    if wire_version != WIRE_PROTOCOL_LEGACY_VERSION {
        error!("Cannot rewrite to wire protocol version {}", wire_version);
        bail!("Invalid wire version.");
    }
    if let Ok(mut net_msg) = NetworkMessage::deserialize(message) {
        if let NetworkPayload::NetworkPacket(ref mut packet) = net_msg.payload {
            if rewrite_outgoing_payload(&mut packet.message)?.is_some() {
                let mut buffer = Vec::with_capacity(packet.message.len() + 64);
                net_msg.serialize(&mut buffer)?;
                return Ok(Some(Arc::from(buffer)));
            }
        }
    }
    Ok(None)
}

/// Rewrite the payload of an outgoing network packet in-place.  This does the
/// following:
///
///   * For non-transaction packets, remove the genesis index if it is 0. If it
///     is non-zero, then this errors, since wire protocol 0 does not support
///     other genesis indexes.
///   * For catch-up requests, remove the version header, which is assumed to be
///     a single 0 byte.  This is not checked, but currently 0 is the only
///     supported version.
fn rewrite_outgoing_payload(payload: &mut Vec<u8>) -> anyhow::Result<Option<()>> {
    if payload.is_empty() {
        bail!("Empty payload.")
    }
    let consensus_type = u8::deserial(&mut Cursor::new(&payload[..1]))?;
    let packet_type = PacketType::try_from(consensus_type)?;

    match packet_type {
        PacketType::Transaction => Ok(None),
        _ => {
            if payload.len() < 5 {
                bail!("Payload too short.")
            }
            let genesis_index = u32::deserial(&mut Cursor::new(&payload[1..5]))?;
            if genesis_index != 0 {
                bail!("Non-zero genesis index.")
            }
            match packet_type {
                PacketType::CatchUpStatus => {
                    // Remove the genesis index (4 bytes) and version header (1 byte: 0)
                    if payload.len() < 6 {
                        bail!("Payload too short.")
                    }
                    payload.copy_within(6.., 1);
                    payload.truncate(payload.len() - 5);
                    Ok(Some(()))
                }
                _ => {
                    // Remove the genesis index (4 bytes)
                    payload.copy_within(5.., 1);
                    payload.truncate(payload.len() - 4);
                    Ok(Some(()))
                }
            }
        }
    }
}

/// Rewrite an incoming network message from the specified wire protocol version
/// to the current version.  Currently, this only supports rewriting from
/// version 0 (WIRE_PROTOCOL_LEGACY_VERSION). A failure indicates that the
/// message could not be rewritten, which would only be the case if
/// the wire protocol version is wrong or the message is a malformed network
/// packet.
pub fn rewrite_incoming(
    wire_version: WireProtocolVersion,
    message: &mut NetworkMessage,
) -> anyhow::Result<()> {
    if wire_version != WIRE_PROTOCOL_LEGACY_VERSION {
        error!("Cannot rewrite from wire protocol version {}", wire_version);
        bail!("Invalid wire version.");
    }

    if let NetworkPayload::NetworkPacket(ref mut packet) = message.payload {
        rewrite_incoming_payload(&mut packet.message)?;
    }
    Ok(())
}

/// Rewrite the payload of an incoming network packet in-place.  This does the
/// following:
///
///   * For non-transaction packets, inserts a genesis index of 0.
///   * For catch-up requests, insert a version header of 0.
#[allow(clippy::reversed_empty_ranges)]
pub fn rewrite_incoming_payload(payload: &mut Vec<u8>) -> anyhow::Result<()> {
    if payload.is_empty() {
        bail!("Empty payload.")
    }
    let consensus_type = u8::deserial(&mut Cursor::new(&payload[..1]))?;
    let packet_type = PacketType::try_from(consensus_type)?;

    match packet_type {
        PacketType::Transaction => {}
        PacketType::CatchUpStatus => {
            // Add genesis index 0 (four 0 bytes) and version header 0 (one 0 byte).
            let slice = &[0, 0, 0, 0, 0];
            payload.splice(1..1, slice.iter().cloned());
        }
        _ => {
            // Add genesis index 0 (four 0 bytes).
            let slice = &[0, 0, 0, 0];
            payload.splice(1..1, slice.iter().cloned());
        }
    }
    Ok(())
}
