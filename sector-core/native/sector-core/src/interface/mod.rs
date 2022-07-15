//! Module implementing the interface for databases and tables.

use rustler::NifStruct;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, fmt, sync::Arc};
use tokio::sync::RwLock;

pub struct Database {
    pub identifier: String,
    pub tables: HashMap<String, Arc<RwLock<Table>>>,
}

impl fmt::Debug for Database {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Database")
            .field("identifier", &self.identifier)
            .field("tables", &self.tables)
            .finish()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, NifStruct)]
#[module = "SectorCore.InterfaceTypes"]
pub struct Table {
    pub identifier: String,
    pub columns: HashMap<String, Column>,
}

#[derive(Debug, Clone, Serialize, Deserialize, NifStruct)]
#[module = "SectorCore.InterfaceTypes"]
pub struct Column {
    pub identifier: String,
}
