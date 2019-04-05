use crate::common::get_current_stamp_b64;

pub mod packet;
pub mod request;
pub mod message;
pub mod buckets;
pub mod response;
pub mod serialization;

pub use self::packet::{ NetworkPacket, NetworkPacketType, NetworkPacketBuilder };
pub use self::message::NetworkMessage;
pub use self::buckets::Buckets;
pub use self::request::NetworkRequest;
pub use self::response::NetworkResponse;

pub const PROTOCOL_NAME: &'static str = "CONCORDIUMP2P";
pub const PROTOCOL_VERSION: &'static str = "001";
pub const PROTOCOL_NODE_ID_LENGTH: usize = 44;
pub const PROTOCOL_PORT_LENGTH: usize = 5;
pub const PROTOCOL_MESSAGE_ID_LENGTH: usize = 44;
pub const PROTOCOL_NETWORK_ID_LENGTH: usize = 5;
pub const PROTOCOL_NETWORK_CONTENT_SIZE_LENGTH: usize = 10;
pub const PROTOCOL_SENT_TIMESTAMP_LENGTH: usize = 12;
pub const PROTOCOL_MESSAGE_TYPE_LENGTH: usize = 4;

pub const PROTOCOL_MESSAGE_TYPE_REQUEST_PING: &str = "0001";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_FINDNODE: &str = "0002";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_HANDSHAKE: &str = "0003";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_GET_PEERS: &str = "0004";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_BANNODE: &str = "0005";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_UNBANNODE: &str = "0006";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_JOINNETWORK: &str = "0007";
pub const PROTOCOL_MESSAGE_TYPE_REQUEST_LEAVENETWORK: &str = "0008";
pub const PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG: &str = "1001";
pub const PROTOCOL_MESSAGE_TYPE_RESPONSE_FINDNODE: &str = "1002";
pub const PROTOCOL_MESSAGE_TYPE_RESPONSE_PEERSLIST: &str = "1003";
pub const PROTOCOL_MESSAGE_TYPE_RESPONSE_HANDSHAKE: &str = "1004";
pub const PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE: &str = "2001";
pub const PROTOCOL_MESSAGE_TYPE_BROADCASTED_MESSAGE: &str = "2002";

pub fn make_header() -> String {
    format!("{}{}{}",
            PROTOCOL_NAME,
            PROTOCOL_VERSION,
            get_current_stamp_b64())
}
