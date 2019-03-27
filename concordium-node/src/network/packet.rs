use crate::common::{ P2PPeer, P2PNodeId, get_current_stamp };
use crate::network:: {
    PROTOCOL_NAME, PROTOCOL_VERSION, PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE,
    PROTOCOL_MESSAGE_TYPE_BROADCASTED_MESSAGE
};

use crate::utils;
use std::sync::{ RwLock };
use rand::rngs::OsRng;
use rand::{ RngCore };

lazy_static! {
    static ref RNG: RwLock<OsRng> = { RwLock::new(OsRng::new().unwrap()) };
}

#[derive(Debug, Clone)]
pub enum NetworkPacket {
    DirectMessage(P2PPeer, String, P2PNodeId, u16, Vec<u8>),
    BroadcastedMessage(P2PPeer, String, u16, Vec<u8>),
}

impl NetworkPacket {
    pub fn serialize(&self) -> Vec<u8> {
        match self {
            NetworkPacket::DirectMessage(_, msgid, receiver, nid, msg) => {
                let mut pkt = format!("{}{}{:016x}{}{:x}{}{:05}{:010}",
                    PROTOCOL_NAME,
                    PROTOCOL_VERSION,
                    get_current_stamp(),
                    PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE,
                    receiver.get_id(),
                    msgid,
                    nid,
                    msg.len()
                ).into_bytes();
                pkt.extend(msg.into_iter());

                pkt
            }
            NetworkPacket::BroadcastedMessage(_, msgid, nid, msg) => {
                let mut pkt = format!("{}{}{:016x}{}{}{:05}{:010}",
                    PROTOCOL_NAME,
                    PROTOCOL_VERSION,
                    get_current_stamp(),
                    PROTOCOL_MESSAGE_TYPE_BROADCASTED_MESSAGE,
                    msgid,
                    nid,
                    msg.len()
                ).into_bytes();
                pkt.extend(msg.into_iter());

                pkt
            }
        }
    }

    pub fn generate_message_id() -> String {
        let mut secure_bytes = vec![0u8; 256];
        match safe_write!(RNG) {
            Ok(mut l) => l.fill_bytes(&mut secure_bytes),
            Err(_) => return String::new()
        }
        utils::to_hex_string(&utils::sha256_bytes(&secure_bytes))
    }
}
