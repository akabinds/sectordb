//! Module implementing the interface for databases, tables, columns, and datatypes.

mod datatypes;

use rustler::{Env, NifStruct, Term};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, fmt, sync::Arc};
use tokio::sync::RwLock;

pub struct Database {
    pub identifier: String,
    pub tables: HashMap<String, Arc<RwLock<Table>>>,
}

impl Database {
    pub fn new(identifier: String) -> Self {
        Self {
            identifier,
            tables: HashMap::new(),
        }
    }
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
#[module = "SectorCore.Native.Interface.Table"]
pub struct Table {
    pub identifier: String,
    pub columns: HashMap<String, Column>,
}

impl Table {
    pub fn new(identifier: String) -> Self {
        Self {
            identifier,
            columns: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, NifStruct)]
#[module = "SectorCore.Native.Interface.Column"]
pub struct Column {
    pub identifier: String,
}

rustler::init!("Elixir.SectorCore.Native", []);
