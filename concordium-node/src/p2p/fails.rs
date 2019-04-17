use failure::Fail;

#[derive(Debug, Fail)]
#[fail(display = "Peer not found")]
pub struct PeerNotFoundError;

#[derive(Debug, Fail)]
#[fail(display = "Peer marked as unreachable, won't try it")]
pub struct UnreachablePeerError;

#[derive(Debug, Fail)]
#[fail(display = "Already connected to peer")]
pub struct DuplicatePeerError;

#[derive(Debug, Fail)]
#[fail(display = "Connection requested by banned node")]
pub struct BannedNodeRequestedConnectionError;

#[derive(Debug, Fail)]
#[fail(display = "Thread join error")]
pub struct JoinError;
