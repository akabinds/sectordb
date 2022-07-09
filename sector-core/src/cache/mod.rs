//! Caching algorithm and implementation for ModDB.
//!
//! A database cache stores the results of commonly-used queries to prevent having to execute expensive queries each time their result is needed.
//! This reduces excessive and unwanted strain on the database.

#[cfg(test)]
use pretty_assertions::assert_eq as pretty_assert_eq;

use serde::{de::DeserializeOwned, Serialize};
use std::{
    alloc::Allocator,
    // sync::Arc,
    cmp::PartialEq,
    collections::{hash_map::RandomState, HashMap, VecDeque},
    fmt,
    hash::{BuildHasher, Hash},
    ops,
    ptr,
    time::{Duration, Instant},
};

use crate::{
    malloc::SectorAlloc,
    util::{SectorError, SectorResult},
};
// use tokio::sync::Mutex as TokioMutex;

/// Builder interface for [`Cache`].
pub struct CacheBuilder {
    protected_store_capacity: usize,
    probationary_store_capacity: usize,
}

impl CacheBuilder {
    fn new(protected_store_capacity: usize, probationary_store_capacity: usize) -> Self {
        Self {
            protected_store_capacity,
            probationary_store_capacity,
        }
    }

    /// Consume the [`CacheBuilder`] and construct the [`Cache`] instance.
    fn build_cache<K, V>(self) -> SectorResult<Cache<K, V>>
    where
        K: Eq + Hash + Send + Sync,
        V: PartialEq + Send + Sync + Serialize + DeserializeOwned,
    {
        if self.protected_store_capacity == 0 || self.probationary_store_capacity == 0 {
            return Err(Box::new(SectorError::CacheError(
                "cache capacity must be greater than 0",
            )));
        }

        Ok(Cache {
            protected_store: RawCache::new(self.protected_store_capacity),
            probationary_store: RawCache::new(self.probationary_store_capacity),
            hits: 0,
            misses: 0,
            _alloc: SectorAlloc::default(),
        })
    }
}

/// Enumeration to describe the status of a cache entry.
#[derive(Debug, Default, Clone, PartialEq)]
enum CacheEntryStatus {
    /// The default status of a cache entry.
    /// This entry resides in the *protected* store.
    #[default]
    Protected,

    /// An entry that resides in the *probationary* store.
    Probationary,

    /// An entry that has been evicted from the cache.
    Evicted,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct CacheEntry<K, V>
where
    K: Eq + Hash + Send + Sync,
    V: PartialEq + Send + Sync + Serialize + DeserializeOwned,
{
    key: K,
    value: V,
    status: CacheEntryStatus,
    last_accessed: Instant,
    prev: *mut CacheEntry<K, V>,
    next: *mut CacheEntry<K, V>,
}

impl<K, V> CacheEntry<K, V>
where
    K: Eq + Hash + Send + Sync,
    V: PartialEq + Send + Sync + Serialize + DeserializeOwned,
{
    fn new(key: K, value: V) -> Self {
        Self {
            key,
            value,
            status: CacheEntryStatus::default(),
            last_accessed: Instant::now(),
            prev: ptr::null_mut(),
            next: ptr::null_mut(),
        }
    }

    fn extract_key(&self) -> &K {
        &self.key
    }

    fn extract_value(&self) -> &V {
        &self.value
    }

    fn set_status(&mut self, status: CacheEntryStatus) {
        self.status = status;
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub(crate) struct KeyRef<K>
where
    K: Eq + Hash + Send + Sync,
{
    key: *const K,
}

impl<K> KeyRef<K>
where
    K: Eq + Hash + Send + Sync,
{
    fn new(key: &K) -> Self {
        Self {
            key: key as *const K,
        }
    }
}

#[derive(Debug)]
pub(crate) struct RawCache<K, V, H = RandomState, A = SectorAlloc>
where
    K: Eq + Hash + Send + Sync,
    V: PartialEq + Send + Sync + Serialize + DeserializeOwned,
    H: BuildHasher + Default,
    A: Allocator,
{
    internal: HashMap<KeyRef<K>, CacheEntry<K, V>, H>,
    head: *mut CacheEntry<K, V>,
    tail: *mut CacheEntry<K, V>,
    capacity: usize,
    _alloc: A,
}

impl<K, V> RawCache<K, V>
where
    K: Eq + Hash + Send + Sync,
    V: PartialEq + Send + Sync + Serialize + DeserializeOwned,
{
    fn new(capacity: usize) -> Self {
        Self {
            internal: HashMap::default(),
            head: ptr::null_mut(),
            tail: ptr::null_mut(),
            capacity,
            _alloc: SectorAlloc::default(),
        }
    }

    #[allow(clippy::let_and_return)]
    fn put(&mut self, key: K, value: V) -> Option<CacheEntry<K, V>> {
        let inserted = (*self).insert(
            KeyRef {
                key: &key as *const K,
            },
            CacheEntry::new(key, value),
        );

        inserted
    }

    fn remove(&mut self, key: &K) -> Option<CacheEntry<K, V>> {
        let removed = (*self).remove_entry(&KeyRef::new(key))?;

        todo!();
    }

    fn transfer(&mut self, key: &K) {
        todo!();
    }

    fn overwrite(&mut self, key: K, value: V) -> Option<CacheEntry<K, V>> {
        let entry_as_mut = (*self).get_mut(&KeyRef::new(&key))?;

        todo!();
    }

    fn evict(&mut self, key: K) -> Option<CacheEntry<K, V>> {
        let to_evict = (*self).get_mut(&KeyRef {
            key: &key as *const K,
        })?;

        todo!();
    }

    fn reserve(&mut self, to_reserve: usize) -> SectorResult<()> {
        (*self)
            .try_reserve(to_reserve)
            .map_err(|_| SectorError::CacheError("failed to reserve additional cache space"))?;

        Ok(())
    }

    fn shrink(&mut self, min: usize) {
        (*self).shrink_to(min);
    }
}

impl<K, V, H> ops::Deref for RawCache<K, V, H>
where
    K: Eq + Hash + Send + Sync,
    V: PartialEq + Send + Sync + Serialize + DeserializeOwned,
    H: BuildHasher + Default,
{
    type Target = HashMap<KeyRef<K>, CacheEntry<K, V>, H>;

    fn deref(&self) -> &Self::Target {
        &self.internal
    }
}

impl<K, V, H> ops::DerefMut for RawCache<K, V, H>
where
    K: Eq + Hash + Send + Sync,
    V: PartialEq + Send + Sync + Serialize + DeserializeOwned,
    H: BuildHasher + Default,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.internal
    }
}

/// A data structure representing ModDB's cache.
///
/// # Thread Safety
///
/// [`Cache`] is thread-safe and is made to be used in asynchronous and IO-bound tasks/operations.
///
/// # Specification
///
/// [`Cache`] is a **sized, segmented LRU (SLRU), in-memory cache**. Segmented LRU is a variant of the LRU cache.
///
/// This cache has a notion of *protected* and *probationary* entries. These entries are stored in the *protected* store and *probationary* store, respectively.
/// Both of these stores are sized. In each store, the *most recently accessed* entries are closer to the tail of the store and the least-recently accessed entries are closer to the head of the store.
/// When a miss occurs, the data is inserted into the *most recently accessed* end of the *probationary* store. This entry will end up becoming the tail of the *probationary* store until there is another miss.
/// When a hit occurs, the data is moved into the *most recently accessed* end of the *protected* store.
///
/// The size of the *protected* store changes based on the current workload.
/// If a hit occurs while the *protected* store is full, the entries nearest to the head will be sent to the *probationary* store.
/// When evicition needs to occur, the *least recently accessed* entries in the *probationary* store will be evicted.
///
/// The cache also persists to the disk, ensuring that nothing is ever lost due to an unexpected situation since the cache is in-memory.
/// Due to this, the cache only stores copies of the queries and their results.
///
/// A key in either of the stores is a *unique* identifier for the query. The value of the key is the result of the query.
///
/// ## User Configuration
///
/// [`Cache`] is user-configurable through its builder interface, [`CacheBuilder`]. More information is provided in [`CacheBuilder`]'s documentation.
pub(crate) struct Cache<K, V, H = RandomState, A = SectorAlloc>
where
    K: Eq + Hash + Send + Sync,
    V: PartialEq + Send + Sync + Serialize + DeserializeOwned,
    H: BuildHasher + Default,
    A: Allocator,
{
    pub(crate) protected_store: RawCache<K, V, H>,
    pub(crate) probationary_store: RawCache<K, V, H>,
    hits: usize,
    misses: usize,
    _alloc: A,
}

impl<K, V> Cache<K, V>
where
    K: Eq + Hash + Send + Sync,
    V: PartialEq + Send + Sync + Serialize + DeserializeOwned,
{
    pub fn new(
        probationary_store_capacity: usize,
        protected_store_capacity: usize,
    ) -> SectorResult<Self> {
        CacheBuilder::new(protected_store_capacity, probationary_store_capacity).build_cache()
    }

    pub fn put_protected(&mut self, key: K, value: V) -> Option<CacheEntry<K, V>> {
        self.protected_store.put(key, value)
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn to_probationary(&mut self, key: K) -> Option<CacheEntry<K, V>> {
        todo!();
    }

    pub fn evict(&mut self, key: K) -> Option<CacheEntry<K, V>> {
        self.probationary_store.evict(key)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
