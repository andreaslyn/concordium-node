use failure::{Backtrace, Fail};

#[derive(Debug, Fail)]
#[fail(display = "Can't build RPC server")]
pub struct ServerBuildError;

#[derive(Debug, Fail)]
#[fail(display = "General RPC error")]
pub struct GeneralRpcError {
    #[cause]
    grpcio_error: grpcio::Error,
    backtrace: Backtrace,
}

impl From<grpcio::Error> for GeneralRpcError {
    fn from(e: grpcio::Error) -> Self {
        GeneralRpcError {
            grpcio_error: e,
            backtrace:    Backtrace::new(),
        }
    }
}
