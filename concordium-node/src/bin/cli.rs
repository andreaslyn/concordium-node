#![recursion_limit = "1024"]
#[cfg(not(target_os = "windows"))]
extern crate grpciounix as grpcio;
#[cfg(target_os = "windows")]
extern crate grpciowin as grpcio;
#[macro_use]
extern crate log;
#[macro_use]
extern crate failure;

// Explicitly defining allocator to avoid future reintroduction of jemalloc
use std::alloc::System;
#[global_allocator]
static A: System = System;

use byteorder::{ByteOrder, NetworkEndian, ReadBytesExt, WriteBytesExt};

use concordium_common::{
    functor::{FilterFunctor, Functorable},
    make_atomic_callback, safe_read, safe_write, spawn_or_die, write_or_die, RelayOrStopEnvelope,
    RelayOrStopReceiver, UCursor,
};
use concordium_consensus::{
    consensus,
    ffi::{
        self,
        PacketType::{self, *},
    },
};
use concordium_global_state::{
    block::{BakedBlock, BlockPtr, PendingBlock},
    common::{sha256, HashBytes, SerializeToBytes, SHA256},
    finalization::{FinalizationMessage, FinalizationRecord},
    tree::SKOV_DATA,
};
use env_logger::{Builder, Env};
use failure::Fallible;
use p2p_client::{
    client::{
        utils as client_utils, FILE_NAME_GENESIS_DATA, FILE_NAME_PREFIX_BAKER_PRIVATE,
        FILE_NAME_SUFFIX_BAKER_PRIVATE,
    },
    common::{P2PNodeId, PeerType},
    configuration,
    connection::network_handler::message_handler::MessageManager,
    db::P2PDB,
    network::{
        NetworkId, NetworkMessage, NetworkPacket, NetworkPacketType, NetworkRequest,
        NetworkResponse,
    },
    p2p::*,
    rpc::RpcServerImpl,
    stats_engine::StatsEngine,
    stats_export_service::{StatsExportService, StatsServiceMode},
    utils,
};

use std::{
    collections::HashMap,
    convert::TryFrom,
    fs::OpenOptions,
    io::{Read, Write},
    net::SocketAddr,
    str,
    sync::{mpsc, Arc, RwLock},
};

const PAYLOAD_TYPE_LENGTH: u64 = 2;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PacketDirection {
    Inbound,
    Outbound,
}

fn get_config_and_logging_setup() -> (configuration::Config, configuration::AppPreferences) {
    // Get config and app preferences
    let conf = configuration::parse_config();
    let app_prefs = configuration::AppPreferences::new(
        conf.common.config_dir.to_owned(),
        conf.common.data_dir.to_owned(),
    );

    // Prepare the logger
    let env = if conf.common.trace {
        Env::default().filter_or("MY_LOG_LEVEL", "trace")
    } else if conf.common.debug {
        Env::default().filter_or("MY_LOG_LEVEL", "debug")
    } else {
        Env::default().filter_or("MY_LOG_LEVEL", "info")
    };

    let mut log_builder = Builder::from_env(env);
    if conf.common.no_log_timestamp {
        log_builder.default_format_timestamp(false);
    }
    log_builder.init();

    p2p_client::setup_panics();

    info!(
        "Starting up {} version {}!",
        p2p_client::APPNAME,
        p2p_client::VERSION
    );
    info!(
        "Application data directory: {:?}",
        app_prefs.get_user_app_dir()
    );
    info!(
        "Application config directory: {:?}",
        app_prefs.get_user_config_dir()
    );

    (conf, app_prefs)
}

fn setup_baker_guards(
    baker: &mut Option<consensus::ConsensusContainer>,
    node: &P2PNode,
    conf: &configuration::Config,
) -> Vec<std::thread::JoinHandle<()>> {
    if let Some(ref mut baker) = baker {
        let network_id = NetworkId::from(conf.common.network_ids[0].to_owned()); // defaulted so there's always first()

        let baker_clone = baker.to_owned();
        let mut node_ref = node.clone();
        let catch_up_thread = spawn_or_die!("Process consensus catch-up requests", {
            use concordium_consensus::consensus::CatchupRequest::*;
            use concordium_global_state::common::DELTA_LENGTH;
            loop {
                match baker_clone.out_queue().recv_catchup() {
                    Ok(RelayOrStopEnvelope::Relay(msg)) => {
                        let (receiver_id, serialized_bytes) = match msg {
                            BlockByHash(receiver_id, ref hash, delta) => {
                                let mut inner_out_bytes = Vec::with_capacity(
                                    PAYLOAD_TYPE_LENGTH as usize
                                        + hash.len()
                                        + DELTA_LENGTH as usize,
                                );
                                inner_out_bytes
                                    .write_u16::<NetworkEndian>(CatchupBlockByHash as u16)
                                    .expect("Can't write to buffer");
                                inner_out_bytes.extend(hash.iter());
                                inner_out_bytes
                                    .write_u64::<NetworkEndian>(delta)
                                    .expect("Can't write to buffer");

                                (P2PNodeId(receiver_id), inner_out_bytes)
                            }
                            FinalizationRecordByHash(receiver_id, ref hash) => {
                                let mut inner_out_bytes =
                                    Vec::with_capacity(PAYLOAD_TYPE_LENGTH as usize + hash.len());
                                inner_out_bytes
                                    .write_u16::<NetworkEndian>(
                                        CatchupFinalizationRecordByHash as u16,
                                    )
                                    .expect("Can't write to buffer");
                                inner_out_bytes.extend(hash.iter());

                                (P2PNodeId(receiver_id), inner_out_bytes)
                            }
                            FinalizationRecordByIndex(receiver_id, index) => {
                                let mut inner_out_bytes =
                                    Vec::with_capacity(PAYLOAD_TYPE_LENGTH as usize + 8);
                                inner_out_bytes
                                    .write_u16::<NetworkEndian>(
                                        CatchupFinalizationRecordByIndex as u16,
                                    )
                                    .expect("Can't write to buffer");
                                inner_out_bytes
                                    .write_u64::<NetworkEndian>(index)
                                    .expect("Can't write to buffer");

                                (P2PNodeId(receiver_id), inner_out_bytes)
                            }
                            FinalizationMessagesByPoint(receiver_id, ref msg) => {
                                let msg_bytes = msg.serialize();
                                let mut inner_out_bytes = Vec::with_capacity(
                                    PAYLOAD_TYPE_LENGTH as usize + msg_bytes.len(),
                                );
                                inner_out_bytes
                                    .write_u16::<NetworkEndian>(FinalizationMessage as u16)
                                    .expect("Can't write to buffer");
                                inner_out_bytes.extend(&*msg_bytes);

                                (P2PNodeId(receiver_id), inner_out_bytes)
                            }
                        };
                        match &node_ref.send_message(
                            Some(receiver_id),
                            network_id,
                            None,
                            serialized_bytes,
                            false,
                        ) {
                            Ok(_) => info!(
                                "Peer {} sent a {} to peer {}",
                                node_ref.id(),
                                msg,
                                receiver_id,
                            ),
                            Err(_) => error!(
                                "Peer {} couldn't send a {} catch-up request to peer {}",
                                node_ref.id(),
                                msg,
                                receiver_id,
                            ),
                        }
                    }
                    Ok(RelayOrStopEnvelope::Stop) => break,
                    Err(_) => error!("Can't read from queue"),
                }
            }
        });

        let baker_clone = baker.to_owned();
        let mut node_ref = node.clone();
        let consensus_block_thread = spawn_or_die!("Process consensus block output", {
            loop {
                match baker_clone.out_queue().recv_block() {
                    Ok(RelayOrStopEnvelope::Relay(block)) => {
                        let block_bytes = block.serialize();
                        let mut out_bytes =
                            Vec::with_capacity(PAYLOAD_TYPE_LENGTH as usize + block_bytes.len());
                        match out_bytes.write_u16::<NetworkEndian>(ffi::PacketType::Block as u16) {
                            Ok(_) => {
                                out_bytes.extend(&*block_bytes);
                                match &node_ref
                                    .send_message(None, network_id, None, out_bytes, true)
                                {
                                    Ok(_) => info!(
                                        "Peer {} broadcasted a block ({:?}) by baker {}",
                                        node_ref.id(),
                                        sha256(&block_bytes),
                                        block.baker_id,
                                    ),
                                    Err(_) => error!(
                                        "Peer {} couldn't broadcast a block ({:?}) by baker {}!",
                                        node_ref.id(),
                                        sha256(&block_bytes),
                                        block.baker_id,
                                    ),
                                }
                            }
                            Err(_) => error!("Can't write type to packet"),
                        }
                    }
                    Ok(RelayOrStopEnvelope::Stop) => break,
                    _ => error!("Error receiving block from the consensus layer"),
                }
            }
        });

        let baker_clone = baker.to_owned();
        let mut node_ref = node.clone();
        let consensus_fin_msg_thread =
            spawn_or_die!("Process consensus finalization message output", {
                loop {
                    match baker_clone.out_queue().recv_finalization() {
                        Ok(RelayOrStopEnvelope::Relay((peer_id_opt, msg))) => {
                            let bytes = msg.serialize();
                            let mut out_bytes =
                                Vec::with_capacity(PAYLOAD_TYPE_LENGTH as usize + bytes.len());
                            match out_bytes.write_u16::<NetworkEndian>(
                                ffi::PacketType::FinalizationMessage as u16,
                            ) {
                                Ok(_) => {
                                    out_bytes.extend(&*bytes);
                                    let res = if let Some(peer_id) = peer_id_opt {
                                        node_ref.send_message(
                                            Some(P2PNodeId(peer_id)),
                                            network_id,
                                            None,
                                            out_bytes,
                                            false,
                                        )
                                    } else {
                                        node_ref
                                            .send_message(None, network_id, None, out_bytes, true)
                                    };

                                    match res {
                                        Ok(_) => {
                                            info!("Peer {} broadcasted a {}", node_ref.id(), msg,)
                                        }
                                        Err(_) => {
                                            error!("Couldn't broadcast a finalization packet!")
                                        }
                                    }
                                }
                                Err(_) => error!("Can't write type to packet"),
                            }
                        }
                        Ok(RelayOrStopEnvelope::Stop) => break,
                        _ => error!("Error receiving finalization packet from the consensus layer"),
                    }
                }
            });

        let baker_clone = baker.to_owned();
        let mut node_ref = node.clone();
        let consensus_fin_rec_thread =
            spawn_or_die!("Process consensus finalization records output", {
                loop {
                    match baker_clone.out_queue().recv_finalization_record() {
                        Ok(RelayOrStopEnvelope::Relay(rec)) => {
                            let bytes = rec.serialize();
                            let rec_info = rec.to_string();

                            let mut out_bytes =
                                Vec::with_capacity(PAYLOAD_TYPE_LENGTH as usize + bytes.len());
                            match out_bytes.write_u16::<NetworkEndian>(
                                ffi::PacketType::FinalizationRecord as u16,
                            ) {
                                Ok(_) => {
                                    out_bytes.extend(&*bytes);
                                    match &node_ref
                                        .send_message(None, network_id, None, out_bytes, true)
                                    {
                                        Ok(_) => info!(
                                            "Peer {} broadcasted a {}",
                                            node_ref.id(),
                                            rec_info
                                        ),
                                        Err(_) => {
                                            error!("Couldn't broadcast a finalization record!")
                                        }
                                    }
                                }
                                Err(_) => error!("Can't write type to packet"),
                            }
                        }
                        Ok(RelayOrStopEnvelope::Stop) => break,
                        _ => error!("Error receiving finalization record from the consensus layer"),
                    }
                }
            });

        vec![
            catch_up_thread,
            consensus_block_thread,
            consensus_fin_msg_thread,
            consensus_fin_rec_thread,
        ]
    } else {
        vec![]
    }
}

fn instantiate_node(
    conf: &configuration::Config,
    app_prefs: &mut configuration::AppPreferences,
    stats_export_service: &Option<Arc<RwLock<StatsExportService>>>,
) -> (
    P2PNode,
    mpsc::Receiver<RelayOrStopEnvelope<Arc<NetworkMessage>>>,
) {
    let (pkt_in, pkt_out) = mpsc::channel::<RelayOrStopEnvelope<Arc<NetworkMessage>>>();
    let node_id = conf.common.id.clone().map_or(
        app_prefs.get_config(configuration::APP_PREFERENCES_PERSISTED_NODE_ID),
        |id| {
            if !app_prefs.set_config(
                configuration::APP_PREFERENCES_PERSISTED_NODE_ID,
                Some(id.clone()),
            ) {
                error!("Failed to persist own node id");
            };
            Some(id)
        },
    );
    let arc_stats_export_service = if let Some(ref service) = stats_export_service {
        Some(Arc::clone(service))
    } else {
        None
    };

    let broadcasting_checks = Arc::new(FilterFunctor::new("Broadcasting_checks"));

    // Thread #1: Read P2PEvents from P2PNode
    let node = if conf.common.debug {
        let (sender, receiver) = mpsc::channel();
        let _guard = spawn_or_die!("Log loop", move || loop {
            if let Ok(msg) = receiver.recv() {
                info!("{}", msg);
            }
        });
        P2PNode::new(
            node_id,
            &conf,
            pkt_in,
            Some(sender),
            PeerType::Node,
            arc_stats_export_service,
            Arc::clone(&broadcasting_checks),
        )
    } else {
        P2PNode::new(
            node_id,
            &conf,
            pkt_in,
            None,
            PeerType::Node,
            arc_stats_export_service,
            Arc::clone(&broadcasting_checks),
        )
    };
    (node, pkt_out)
}

#[cfg(feature = "benchmark")]
fn tps_setup_process_output(cli: &configuration::CliConfig) -> (bool, u64) {
    (cli.tps.enable_tps_test, cli.tps.tps_message_count)
}

#[cfg(not(feature = "benchmark"))]
fn tps_setup_process_output(_: &configuration::CliConfig) -> (bool, u64) { (false, 0) }

fn handle_pkt_out(
    node: &mut P2PNode,
    baker: &mut consensus::ConsensusContainer,
    peer_id: P2PNodeId,
    network_id: NetworkId,
    mut msg: UCursor,
) -> Fallible<()> {
    use concordium_global_state::{common::DELTA_LENGTH, tree::SKOV_DATA};

    ensure!(
        msg.len() >= msg.position() + PAYLOAD_TYPE_LENGTH,
        "Message needs at least {} bytes",
        PAYLOAD_TYPE_LENGTH
    );

    let consensus_type = msg.read_u16::<NetworkEndian>()?;
    let view = msg.read_all_into_view()?;
    let content = &view.as_slice()[PAYLOAD_TYPE_LENGTH as usize..];
    let packet_type = PacketType::try_from(consensus_type)?;

    let is_unique = match packet_type {
        Block => {
            let pending_block = PendingBlock::new(content)?;

            // don't pattern match directly in order to release the lock quickly
            let result = if let Ok(ref mut skov) = safe_write!(SKOV_DATA) {
                skov.add_block(pending_block.clone())
            } else {
                error!("Can't obtain a write lock on Skov!");
                Ok(None) // temporary placeholder; we don't want to suggest a duplicate
            };

            match result {
                Ok(Some(_)) => false,
                Ok(None) => true,
                Err(e) => {
                    let e = e.to_string();
                    if e == "MissingParent" {
                        let mut inner_out_bytes = Vec::with_capacity(
                            pending_block.block.pointer.len() + DELTA_LENGTH as usize,
                        );
                        inner_out_bytes.extend_from_slice(&pending_block.block.pointer);
                        inner_out_bytes
                            .write_u64::<NetworkEndian>(0u64)
                            .expect("Can't write to buffer");
                        send_catchup_request_block_by_hash_to_consensus(
                            baker,
                            node,
                            peer_id,
                            network_id,
                            &inner_out_bytes,
                            PacketDirection::Outbound,
                        )?;
                        true
                    } else {
                        true
                    }
                }
            }
        }
        FinalizationRecord => {
            let record = FinalizationRecord::deserialize(content)?;

            if let Ok(ref mut skov) = SKOV_DATA.write() {
                skov.add_finalization(record)
            } else {
                error!("Can't obtain a write lock on Skov!");
                true // temporary placeholder; we don't want to suggest a duplicate
            }
        }
        _ => true,
    };

    if !is_unique {
        warn!("Peer {} sent us a duplicate {}", peer_id, packet_type,);
    } else {
        if let Err(e) =
            send_msg_to_consensus(node, baker, peer_id, network_id, packet_type, content)
        {
            error!("Send network message to baker has failed: {:?}", e);
        }
    }

    Ok(())
}

fn send_msg_to_consensus(
    node: &mut P2PNode,
    baker: &mut consensus::ConsensusContainer,
    peer_id: P2PNodeId,
    network_id: NetworkId,
    packet_type: PacketType,
    content: &[u8],
) -> Fallible<()> {
    use concordium_global_state::common::DELTA_LENGTH;

    match packet_type {
        Block => send_block_to_consensus(baker, peer_id, content),
        Transaction => send_transaction_to_consensus(baker, peer_id, content),
        FinalizationMessage => send_finalization_message_to_consensus(baker, peer_id, content),
        FinalizationRecord => send_finalization_record_to_consensus(baker, peer_id, content),
        CatchupBlockByHash => {
            ensure!(
                content.len() == SHA256 as usize + DELTA_LENGTH as usize,
                "{} needs {} bytes",
                CatchupBlockByHash,
                SHA256 + DELTA_LENGTH,
            );
            send_catchup_request_block_by_hash_to_consensus(
                baker,
                node,
                peer_id,
                network_id,
                content,
                PacketDirection::Inbound,
            )
        }
        CatchupFinalizationRecordByHash => {
            ensure!(
                content.len() == SHA256 as usize,
                "{} needs {} bytes",
                CatchupFinalizationRecordByHash,
                SHA256
            );
            send_catchup_request_finalization_record_by_hash_to_consensus(
                baker,
                node,
                peer_id,
                network_id,
                content,
                PacketDirection::Inbound,
            )
        }
        CatchupFinalizationRecordByIndex => {
            ensure!(
                content.len() == 8,
                "{} needs {} bytes",
                CatchupFinalizationRecordByIndex,
                8
            );
            send_catchup_request_finalization_record_by_index_to_consensus(
                baker,
                node,
                peer_id,
                network_id,
                content,
                PacketDirection::Inbound,
            )
        }
        CatchupFinalizationMessagesByPoint => {
            send_catchup_finalization_messages_by_point_to_consensus(baker, peer_id, content)
        }
    }
}

fn setup_process_output(
    node: &P2PNode,
    db: &P2PDB,
    conf: &configuration::Config,
    rpc_serv: &Option<RpcServerImpl>,
    baker: &mut Option<consensus::ConsensusContainer>,
    pkt_out: RelayOrStopReceiver<Arc<NetworkMessage>>,
) -> std::thread::JoinHandle<()> {
    let mut _node_self_clone = node.clone();
    let mut _db = db.clone();
    let _no_trust_bans = conf.common.no_trust_bans;
    let _no_trust_broadcasts = conf.connection.no_trust_broadcasts;
    let mut _rpc_clone = rpc_serv.clone();
    let _desired_nodes_clone = conf.connection.desired_nodes;
    let _test_runner_url = conf.cli.test_runner_url.clone();
    let mut _baker_pkt_clone = baker.clone();
    let mut _stats_engine = StatsEngine::new(&conf.cli);
    let mut _msg_count = 0;
    let (_tps_test_enabled, _tps_message_count) = tps_setup_process_output(&conf.cli);

    let _network_id = NetworkId::from(conf.common.network_ids[0].to_owned()); // defaulted so there's always first()

    let guard_pkt = spawn_or_die!("Higher queue processing", {
        while let Ok(RelayOrStopEnvelope::Relay(full_msg)) = pkt_out.recv() {
            match *full_msg {
                NetworkMessage::NetworkRequest(
                    NetworkRequest::BanNode(ref peer, peer_to_ban),
                    ..
                ) => {
                    utils::ban_node(
                        &mut _node_self_clone,
                        peer,
                        peer_to_ban,
                        &_db,
                        _no_trust_bans,
                    );
                }
                NetworkMessage::NetworkRequest(
                    NetworkRequest::UnbanNode(ref peer, peer_to_ban),
                    ..
                ) => {
                    utils::unban_node(
                        &mut _node_self_clone,
                        peer,
                        peer_to_ban,
                        &_db,
                        _no_trust_bans,
                    );
                }
                NetworkMessage::NetworkResponse(
                    NetworkResponse::PeerList(ref peer, ref peers),
                    ..
                ) => {
                    debug!("Received PeerList response, attempting to satisfy desired peers");
                    let mut new_peers = 0;
                    let peer_count = _node_self_clone
                        .get_peer_stats(&[])
                        .iter()
                        .filter(|x| x.peer_type == PeerType::Node)
                        .count();
                    for peer_node in peers {
                        debug!(
                            "Peer {}/{}/{} sent us peer info for {}/{}/{}",
                            peer.id(),
                            peer.ip(),
                            peer.port(),
                            peer_node.id(),
                            peer_node.ip(),
                            peer_node.port()
                        );
                        if _node_self_clone
                            .connect(PeerType::Node, peer_node.addr, Some(peer_node.id()))
                            .map_err(|e| debug!("{}", e))
                            .is_ok()
                        {
                            new_peers += 1;
                        }
                        if new_peers + peer_count as u8 >= _desired_nodes_clone {
                            break;
                        }
                    }
                }
                NetworkMessage::NetworkPacket(ref pac, ..) => {
                    match pac.packet_type {
                        NetworkPacketType::DirectMessage(..) => {
                            if _tps_test_enabled {
                                _stats_engine.add_stat(pac.message.len() as u64);
                                _msg_count += 1;

                                if _msg_count == _tps_message_count {
                                    info!(
                                        "TPS over {} messages is {}",
                                        _tps_message_count,
                                        _stats_engine.calculate_total_tps_average()
                                    );
                                    _msg_count = 0;
                                    _stats_engine.clear();
                                }
                            };
                        }
                        NetworkPacketType::BroadcastedMessage => {
                            if let Some(ref testrunner_url) = _test_runner_url {
                                send_packet_to_testrunner(&_node_self_clone, testrunner_url, pac);
                            };
                        }
                    };

                    if let Some(ref mut baker) = _baker_pkt_clone {
                        if let Err(e) = handle_pkt_out(
                            &mut _node_self_clone,
                            baker,
                            pac.peer.id(),
                            _network_id,
                            pac.message.clone(),
                        ) {
                            error!("There's an issue with an outbound packet: {}", e);
                        }
                    }
                }
                NetworkMessage::NetworkRequest(NetworkRequest::Retransmit(..), ..) => {
                    panic!("Not implemented yet");
                }
                _ => {}
            }
        }
    });

    info!(
        "Concordium P2P layer. Network disabled: {}",
        conf.cli.no_network
    );

    guard_pkt
}

fn send_transaction_to_consensus(
    baker: &mut consensus::ConsensusContainer,
    peer_id: P2PNodeId,
    content: &[u8],
) -> Fallible<()> {
    baker.send_transaction(content);
    info!("Peer {} sent a transaction to the consensus layer", peer_id);
    Ok(())
}

fn send_finalization_record_to_consensus(
    baker: &mut consensus::ConsensusContainer,
    peer_id: P2PNodeId,
    content: &[u8],
) -> Fallible<()> {
    let record = FinalizationRecord::deserialize(content)?;

    match baker.send_finalization_record(peer_id.as_raw(), &record) {
        0i64 => info!("Peer {} sent a {} to consensus", peer_id, record),
        err_code => error!(
            "Peer {} can't send a finalization record to consensus due to error code #{} (bytes: \
             {:?}, length: {})",
            peer_id,
            err_code,
            content,
            content.len(),
        ),
    }

    Ok(())
}

fn send_finalization_message_to_consensus(
    baker: &mut consensus::ConsensusContainer,
    peer_id: P2PNodeId,
    content: &[u8],
) -> Fallible<()> {
    let message = FinalizationMessage::deserialize(content)?;

    baker.send_finalization(peer_id.as_raw(), &message);
    info!("Peer {} sent a {} to the consensus layer", peer_id, message);

    Ok(())
}

fn send_block_to_consensus(
    baker: &mut consensus::ConsensusContainer,
    peer_id: P2PNodeId,
    content: &[u8],
) -> Fallible<()> {
    let baked_block = BakedBlock::deserialize(content)?;

    // send unique blocks to the consensus layer
    match baker.send_block(peer_id.as_raw(), &baked_block) {
        0i64 => info!(
            "Peer {} sent a block ({:?}) to consensus",
            peer_id,
            sha256(content),
        ),
        err_code => error!(
            "Peer {} can't send block from network to consensus due to error code #{} (bytes: \
             {:?}, length: {})",
            peer_id,
            err_code,
            content,
            content.len(),
        ),
    }

    Ok(())
}

// Upon handshake completion we ask the consensus layer for a finalization point
// we want to catchup from. This information is relayed to the peer we just
// connected to, which will then emit all finalizations past this point.
fn send_catchup_finalization_messages_by_point_to_consensus(
    baker: &mut consensus::ConsensusContainer,
    peer_id: P2PNodeId,
    content: &[u8],
) -> Fallible<()> {
    match baker.get_finalization_messages(content, peer_id.as_raw())? {
        0i64 => info!(
            "Peer {} requested finalization messages by point from consensus",
            peer_id
        ),
        err_code => error!(
            "Peer {} could not request finalization messages by point from consensus due to error \
             code {} (bytes: {:?}, length: {})",
            peer_id,
            err_code,
            content,
            content.len(),
        ),
    }
    Ok(())
}

macro_rules! send_catchup_request_to_consensus {
    (
        $req_type:expr,
        $node:ident,
        $baker:ident,
        $content:ident,
        $peer_id:ident,
        $network_id:ident,
        $consensus_req_call:expr,
        $packet_direction:expr,
    ) => {{
        debug!("Got a consensus catch-up request for \"{}\"", $req_type);

        if $packet_direction == PacketDirection::Inbound {
            let res = $consensus_req_call($baker, $content)?;
            let return_type = match $req_type {
                CatchupBlockByHash => Block,
                CatchupFinalizationRecordByHash => FinalizationRecord,
                CatchupFinalizationRecordByIndex => FinalizationRecord,
                catchall_val => panic!("Can't respond to catchup type {}", catchall_val),
            };

            if !res.is_empty() && NetworkEndian::read_u64(&res[..8]) > 0 {
                let mut out_bytes = Vec::with_capacity(PAYLOAD_TYPE_LENGTH as usize + res.len());
                out_bytes
                    .write_u16::<NetworkEndian>(return_type as u16)
                    .expect("Can't write to buffer");
                out_bytes.extend(res);

                match &$node.send_message(Some($peer_id), $network_id, None, out_bytes, false) {
                    Ok(_) => info!(
                        "Responded to a catch-up request type \"{}\" from peer {}",
                        $req_type, $peer_id
                    ),
                    Err(_) => error!(
                        "Couldn't respond to a catch-up request type \"{}\" from peer {}!",
                        $req_type, $peer_id
                    ),
                }
            } else {
                error!(
                    "Consensus doesn't have the data to fulfill a catch-up request type \"{}\" \
                     (to obtain a \"{}\") that peer {} requested (response: {:?})",
                    $req_type, return_type, $peer_id, res
                );
            }
        } else {
            let mut out_bytes = Vec::with_capacity(PAYLOAD_TYPE_LENGTH as usize + $content.len());
            out_bytes
                .write_u16::<NetworkEndian>($req_type as u16)
                .expect("Can't write to buffer");
            out_bytes.extend($content);

            match &$node.send_message(Some($peer_id), $network_id, None, out_bytes, false) {
                Ok(_) => info!(
                    "Sent a catch-up request type \"{}\" to peer {}",
                    $req_type, $peer_id
                ),
                Err(_) => error!(
                    "Couldn't respond to a catch-up request type \"{}\" to peer {}!",
                    $req_type, $peer_id
                ),
            }
        }

        Ok(())
    }};
}

// This function requests the finalization record for a certain finalization
// index (this function is triggered by consensus on another peer actively asks
// the p2p layer to request this for it)
fn send_catchup_request_finalization_record_by_index_to_consensus(
    baker: &mut consensus::ConsensusContainer,
    node: &mut P2PNode,
    peer_id: P2PNodeId,
    network_id: NetworkId,
    content: &[u8],
    direction: PacketDirection,
) -> Fallible<()> {
    send_catchup_request_to_consensus!(
        ffi::PacketType::CatchupFinalizationRecordByIndex,
        node,
        baker,
        content,
        peer_id,
        network_id,
        |baker: &consensus::ConsensusContainer, content: &[u8]| -> Fallible<Vec<u8>> {
            let index = NetworkEndian::read_u64(&content[..8]);
            baker.get_indexed_finalization(index)
        },
        direction,
    )
}

fn send_catchup_request_finalization_record_by_hash_to_consensus(
    baker: &mut consensus::ConsensusContainer,
    node: &mut P2PNode,
    peer_id: P2PNodeId,
    network_id: NetworkId,
    content: &[u8],
    direction: PacketDirection,
) -> Fallible<()> {
    // extra debug
    if let Ok(skov) = safe_read!(SKOV_DATA) {
        let hash = HashBytes::new(content);
        if skov.get_finalization_record_by_hash(&hash).is_some() {
            info!(
                "Peer {} here; I do have the finalization record for block {:?}",
                node.id(),
                hash
            );
        }
    } else {
        error!("Can't obtain a read lock on Skov!");
    }

    send_catchup_request_to_consensus!(
        ffi::PacketType::CatchupFinalizationRecordByHash,
        node,
        baker,
        content,
        peer_id,
        network_id,
        |baker: &consensus::ConsensusContainer, content: &[u8]| -> Fallible<Vec<u8>> {
            baker.get_block_finalization(content)
        },
        direction,
    )
}

fn send_catchup_request_block_by_hash_to_consensus(
    baker: &mut consensus::ConsensusContainer,
    node: &mut P2PNode,
    peer_id: P2PNodeId,
    network_id: NetworkId,
    content: &[u8],
    direction: PacketDirection,
) -> Fallible<()> {
    use concordium_global_state::common::{DELTA_LENGTH, SHA256};
    // extra debug
    let hash = &content[..SHA256 as usize];
    let delta = NetworkEndian::read_u64(&content[SHA256 as usize..][..DELTA_LENGTH as usize]);

    add_block_to_skov(node.id(), &hash);

    if delta == 0 {
        send_catchup_request_to_consensus!(
            ffi::PacketType::CatchupBlockByHash,
            node,
            baker,
            content,
            peer_id,
            network_id,
            |baker: &consensus::ConsensusContainer, content: &[u8]| -> Fallible<Vec<u8>> {
                baker.get_block(content)
            },
            direction,
        )
    } else {
        send_catchup_request_to_consensus!(
            ffi::PacketType::CatchupBlockByHash,
            node,
            baker,
            content,
            peer_id,
            network_id,
            |baker: &consensus::ConsensusContainer, _: &[u8]| -> Fallible<Vec<u8>> {
                baker.get_block_by_delta(hash, delta)
            },
            direction,
        )
    }
}

fn add_block_to_skov(node_id: P2PNodeId, hash_bytes: &[u8]) {
    if let Ok(skov) = safe_read!(SKOV_DATA) {
        let hash = HashBytes::new(&hash_bytes);
        if skov.get_block_by_hash(&hash).is_some() {
            info!("Peer {} here; I do have block {:?}", node_id, hash);
        }
    } else {
        error!("Can't obtain a read lock on Skov!");
    }
}

fn main() -> Fallible<()> {
    let (conf, mut app_prefs) = get_config_and_logging_setup();
    if conf.common.print_config {
        // Print out the configuration
        info!("{:?}", conf);
    }

    // Retrieving bootstrap nodes
    let dns_resolvers =
        utils::get_resolvers(&conf.connection.resolv_conf, &conf.connection.dns_resolver);
    for resolver in &dns_resolvers {
        debug!("Using resolver: {}", resolver);
    }
    let bootstrap_nodes = utils::get_bootstrap_nodes(
        conf.connection.bootstrap_server.clone(),
        &dns_resolvers,
        conf.connection.dnssec_disabled,
        &conf.connection.bootstrap_node,
    );

    // Create the database
    let mut db_path = app_prefs.get_user_app_dir();
    db_path.push("p2p.db");
    let db = P2PDB::new(db_path.as_path());

    // Instantiate stats export engine
    let stats_export_service =
        client_utils::instantiate_stats_export_engine(&conf, StatsServiceMode::NodeMode)
            .unwrap_or_else(|e| {
                error!(
                    "I was not able to instantiate an stats export service: {}",
                    e
                );
                None
            });

    info!("Debugging enabled: {}", conf.common.debug);

    // Instantiate the p2p node
    let (mut node, pkt_out) = instantiate_node(&conf, &mut app_prefs, &stats_export_service);

    // Banning nodes in database
    match db.get_banlist() {
        Some(nodes) => {
            info!("Found existing banlist, loading up!");
            for n in nodes {
                node.ban_node(n);
            }
        }
        None => {
            warn!("Couldn't find existing banlist. Creating new!");
            db.create_banlist();
        }
    };

    #[cfg(feature = "instrumentation")]
    // Start push gateway to prometheus
    client_utils::start_push_gateway(&conf.prometheus, &stats_export_service, node.id())?;

    // Start the P2PNode
    //
    // Thread #3: P2P event loop
    node.spawn();

    // Connect to nodes (args and bootstrap)
    if !conf.cli.no_network {
        info!("Concordium P2P layer, Network disabled");
        create_connections_from_config(&conf.connection, &dns_resolvers, &mut node);
        if !conf.connection.no_bootstrap_dns {
            info!("Attempting to bootstrap");
            bootstrap(&bootstrap_nodes, &mut node);
        }
    }

    let mut baker = if conf.cli.baker.baker_id.is_some() {
        // Starting baker
        start_baker(&node, &conf.cli.baker, &app_prefs)
    } else {
        None
    };

    if let Some(ref baker) = baker {
        // Register handler for sending out a consensus catch-up request after handshake
        // by finalization point.
        let cloned_handshake_response_node = Arc::new(RwLock::new(node.clone()));
        let baker_clone = baker.clone();
        let message_handshake_response_handler = &node.message_handler();
        safe_write!(message_handshake_response_handler)?.add_response_callback(
            make_atomic_callback!(move |msg: &NetworkResponse| {
                if let NetworkResponse::Handshake(ref remote_peer, ref nets, _) = msg {
                    if remote_peer.peer_type() == PeerType::Node {
                        if let Some(net) = nets.iter().next() {
                            let mut locked_cloned_node =
                                write_or_die!(cloned_handshake_response_node);
                            if let Ok(bytes) = baker_clone.get_finalization_point() {
                                let mut out_bytes =
                                    Vec::with_capacity(PAYLOAD_TYPE_LENGTH as usize + bytes.len());
                                match out_bytes.write_u16::<NetworkEndian>(
                                    ffi::PacketType::CatchupFinalizationMessagesByPoint as u16,
                                ) {
                                    Ok(_) => {
                                        out_bytes.extend(&bytes);
                                        match locked_cloned_node.send_message(
                                            Some(remote_peer.id()),
                                            *net,
                                            None,
                                            out_bytes,
                                            false,
                                        ) {
                                            Ok(_) => info!(
                                                "Peer {} requested finalization messages by point \
                                                 from peer {}",
                                                locked_cloned_node.id(),
                                                remote_peer.id()
                                            ),
                                            Err(_) => error!(
                                                "Peer {} couldn't send a catch-up request for \
                                                 finalization messages by point!",
                                                locked_cloned_node.id(),
                                            ),
                                        }
                                    }
                                    Err(_) => error!(
                                        "Can't write type to packet {}",
                                        ffi::PacketType::CatchupFinalizationMessagesByPoint
                                    ),
                                }
                            }
                        } else {
                            error!(
                                "Handshake without network, so can't ask for finalization messages"
                            );
                        }
                    }
                }
                Ok(())
            }),
        );
    }

    // Starting rpc server
    let mut rpc_serv = if !conf.cli.rpc.no_rpc_server {
        let mut serv = RpcServerImpl::new(node.clone(), db.clone(), baker.clone(), &conf.cli.rpc);
        serv.start_server()?;
        Some(serv)
    } else {
        None
    };

    // Connect outgoing messages to be forwarded into the baker and RPC streams.
    //
    // Thread #4: Read P2PNode output
    let higer_process_thread =
        setup_process_output(&node, &db, &conf, &rpc_serv, &mut baker, pkt_out);

    // Create listeners on baker output to forward to P2PNode
    //
    // Threads #5, #6, #7, #8, #9
    let ths = setup_baker_guards(&mut baker, &node, &conf);

    // Wait for node closing
    node.join().expect("Node thread panicked!");

    // Close baker if present
    if let Some(ref mut baker_ref) = baker {
        if let Some(baker_id) = conf.cli.baker.baker_id {
            baker_ref.stop_baker(baker_id)
        };
        ffi::stop_haskell();
    }

    // Wait for the threads to stop
    higer_process_thread
        .join()
        .expect("Higher process thread panicked");
    for th in ths {
        th.join().expect("Baker sub-thread panicked");
    }

    // Close rpc server if present
    if let Some(ref mut serv) = rpc_serv {
        serv.stop_server()?;
    }

    // Close stats server export if present
    client_utils::stop_stats_export_engine(&conf, &stats_export_service);

    Ok(())
}

fn start_baker(
    node: &P2PNode,
    conf: &configuration::BakerConfig,
    app_prefs: &configuration::AppPreferences,
) -> Option<consensus::ConsensusContainer> {
    conf.baker_id.and_then(|baker_id| {
        // Check for invalid configuration
        if baker_id > conf.baker_num_bakers {
            // Baker ID is higher than amount of bakers in the network. Bail!
            error!("Baker ID is higher than the number of bakers in the network! Disabling baking");
            return None;
        }

        info!("Starting up baker thread");
        ffi::start_haskell();

        match get_baker_data(app_prefs, conf) {
            Ok((genesis_data, private_data)) => {
                let genesis_ptr = BlockPtr::genesis(&genesis_data);
                info!(
                    "Peer {} has genesis data with hash {:?} and block hash {:?}",
                    node.id(),
                    sha256(&genesis_data),
                    genesis_ptr.hash,
                );
                safe_write!(SKOV_DATA)
                    .expect("Couldn't write the genesis data to Skov!")
                    .add_genesis(genesis_ptr);

                let mut consensus_runner = consensus::ConsensusContainer::default();
                consensus_runner.start_baker(baker_id, genesis_data, private_data);

                Some(consensus_runner)
            }
            Err(_) => {
                error!("Can't read needed data...");
                None
            }
        }
    })
}

fn bootstrap(bootstrap_nodes: &Result<Vec<SocketAddr>, &'static str>, node: &mut P2PNode) {
    match bootstrap_nodes {
        Ok(nodes) => {
            for &addr in nodes {
                info!("Found bootstrap node: {}", addr);
                node.connect(PeerType::Bootstrapper, addr, None)
                    .unwrap_or_else(|e| error!("{}", e));
            }
        }
        Err(e) => error!("Couldn't retrieve bootstrap node list! {:?}", e),
    };
}

fn create_connections_from_config(
    conf: &configuration::ConnectionConfig,
    dns_resolvers: &[String],
    node: &mut P2PNode,
) {
    for connect_to in &conf.connect_to {
        match utils::parse_host_port(&connect_to, &dns_resolvers, conf.dnssec_disabled) {
            Ok(addrs) => {
                for addr in addrs {
                    info!("Connecting to peer {}", &connect_to);
                    node.connect(PeerType::Node, addr, None)
                        .unwrap_or_else(|e| debug!("{}", e));
                }
            }
            Err(err) => error!("Can't parse data for node to connect to {}", err),
        }
    }
}

#[cfg(feature = "instrumentation")]
fn send_packet_to_testrunner(node: &P2PNode, test_runner_url: &str, pac: &NetworkPacket) {
    debug!("Sending information to test runner");
    match reqwest::get(&format!(
        "{}/register/{}/{}",
        test_runner_url,
        node.id(),
        pac.message_id
    )) {
        Ok(ref mut res) if res.status().is_success() => {
            info!("Registered packet received with test runner")
        }
        _ => error!("Couldn't register packet received with test runner"),
    }
}

#[cfg(not(feature = "instrumentation"))]
fn send_packet_to_testrunner(_: &P2PNode, _: &str, _: &NetworkPacket) {}

fn _send_retransmit_packet(
    node: &mut P2PNode,
    receiver: P2PNodeId,
    network_id: NetworkId,
    message_id: &str,
    payload_type: u16,
    data: &[u8],
) {
    let mut out_bytes = Vec::with_capacity(2 + data.len());
    match out_bytes.write_u16::<NetworkEndian>(payload_type as u16) {
        Ok(_) => {
            out_bytes.extend(data);
            match node.send_message(
                Some(receiver),
                network_id,
                Some(message_id.to_owned()),
                out_bytes,
                false,
            ) {
                Ok(_) => debug!("Retransmitted packet of type {}", payload_type),
                Err(_) => error!("Couldn't retransmit packet of type {}!", payload_type),
            }
        }
        Err(_) => error!("Can't write payload type, so failing retransmit of packet"),
    }
}

fn get_baker_data(
    app_prefs: &configuration::AppPreferences,
    conf: &configuration::BakerConfig,
) -> Fallible<(Vec<u8>, Vec<u8>)> {
    let mut genesis_loc = app_prefs.get_user_app_dir();
    genesis_loc.push(FILE_NAME_GENESIS_DATA);

    let mut private_loc = app_prefs.get_user_app_dir();

    if let Some(baker_id) = conf.baker_id {
        private_loc.push(format!(
            "{}{}{}",
            FILE_NAME_PREFIX_BAKER_PRIVATE, baker_id, FILE_NAME_SUFFIX_BAKER_PRIVATE
        ))
    };

    let (generated_genesis, generated_private_data) =
        if !genesis_loc.exists() || !private_loc.exists() {
            consensus::ConsensusContainer::generate_data(conf.baker_genesis, conf.baker_num_bakers)?
        } else {
            (vec![], HashMap::new())
        };

    let given_genesis = if !genesis_loc.exists() {
        match OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&genesis_loc)
        {
            Ok(mut file) => match file.write_all(&generated_genesis) {
                Ok(_) => generated_genesis,
                Err(_) => bail!("Couldn't write out genesis data"),
            },
            Err(_) => bail!("Couldn't open up genesis file for writing"),
        }
    } else {
        match OpenOptions::new().read(true).open(&genesis_loc) {
            Ok(mut file) => {
                let mut read_data = vec![];
                match file.read_to_end(&mut read_data) {
                    Ok(_) => read_data.clone(),
                    Err(_) => bail!("Couldn't read genesis file properly"),
                }
            }
            _ => bail!("Unexpected"),
        }
    };

    let given_private_data = if !private_loc.exists() {
        match OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&private_loc)
        {
            Ok(mut file) => {
                if let Some(baker_id) = conf.baker_id {
                    match file.write_all(&generated_private_data[&(baker_id as i64)]) {
                        Ok(_) => generated_private_data[&(baker_id as i64)].to_owned(),
                        Err(_) => bail!("Couldn't write out private baker data"),
                    }
                } else {
                    bail!("Couldn't write out private baker data");
                }
            }
            Err(_) => bail!("Couldn't open up private baker file for writing"),
        }
    } else {
        match OpenOptions::new().read(true).open(&private_loc) {
            Ok(mut file) => {
                let mut read_data = vec![];
                match file.read_to_end(&mut read_data) {
                    Ok(_) => read_data,
                    Err(_) => bail!("Couldn't open up private baker file for reading"),
                }
            }
            _ => bail!("Unexpected"),
        }
    };

    Ok((given_genesis, given_private_data))
}
