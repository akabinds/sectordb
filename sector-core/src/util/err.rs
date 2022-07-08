pub type SectorResult<T, E = SectorError> = Result<T, E>;

#[non_exhaustive]
#[derive(Debug, thiserror::Error)]
pub enum SectorError {
    #[error("failed to parse at line {line} on column {column}: {msg:?}")]
    ParseError {
        line: usize,
        column: usize,
        msg: &'static str,
    },
    #[error("cache error: {msg:?}")]
    CacheError { msg: &'static str },
    #[error(transparent)]
    IoError(#[from] tokio::io::Error),
    #[error(transparent)]
    AllocError(#[from] std::alloc::AllocError),
}

impl SectorError {
    pub fn parse_error(line: usize, column: usize, msg: &'static str) -> Self {
        Self::ParseError { line, column, msg }
    }

    pub fn cache_error(msg: &'static str) -> Self {
        Self::CacheError { msg }
    }

    pub fn io_error(err: tokio::io::Error) -> Self {
        Self::IoError(err)
    }

    pub fn alloc_error(err: std::alloc::AllocError) -> Self {
        Self::AllocError(err)
    }
}
