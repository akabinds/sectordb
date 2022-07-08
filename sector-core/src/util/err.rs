pub type SectorResult<T, E = SectorError> = Result<T, Box<E>>;

#[non_exhaustive]
#[derive(Debug, thiserror::Error)]
pub enum SectorError {
    #[error("failed to parse at line {1} on column {2}: {0:?}")]
    ParseError(&'static str, usize, usize),
    #[error("cache error: {0:?}")]
    CacheError(&'static str),
    #[error(transparent)]
    IoError(#[from] tokio::io::Error),
    #[error(transparent)]
    AllocError(#[from] std::alloc::AllocError),
}
