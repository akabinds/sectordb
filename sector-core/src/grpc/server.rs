use crate::util::SectorResult;
use tonic::{transport::Server, Request, Response, Status};
use transaction::{
    transaction_server::Transaction, CommittedTransactionResponse, DbTransaction,
    TransactionMetadata,
};

mod transaction {
    tonic::include_proto!("transaction");
}

#[derive(Debug, Default)]
pub struct TransactionService {}

// find a way to make these functions work with `SectorResult` return type
#[tonic::async_trait]
impl Transaction for TransactionService {
    async fn init(
        &self,
        metadata: Request<TransactionMetadata>,
    ) -> Result<Response<DbTransaction>, Status> {
        Ok(Response::new(DbTransaction {
            metadata: Some(metadata.into_inner()),
        }))
    }

    async fn commit(
        &self,
        transaction: Request<DbTransaction>,
    ) -> Result<Response<CommittedTransactionResponse>, Status> {
        todo!();
    }
}
