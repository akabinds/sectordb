//! Module implementing the interface for databases and tables.

use serde::{Deserialize, Serialize};
use std::{fmt, sync::Arc};
use tokio::sync::RwLock;

// try to find a way to prevent from having to make `Database` and `Table` generic, if possible
pub struct Database<T> {
    pub identifier: String,
    pub tables: Vec<Arc<RwLock<Table<T>>>>,
}

impl<T: fmt::Debug> fmt::Debug for Database<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Database")
            .field("identifier", &self.identifier)
            .field("tables", &self.tables)
            .finish()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Table<T> {
    pub identifier: String,
    columns: Vec<Column<T>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Column<T> {
    pub identifier: String,
    _type: T,
}
