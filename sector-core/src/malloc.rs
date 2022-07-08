//! Memory allocator for SectorDB.
//!
//! This module provides implementation for the memory allocator for SectorDB. Memory can be allocated for various purposes,
//! such as creating a new table, temporarily allocating for a new `Cache` instance, and many more.

use crate::util::{SectorError, SectorResult};
use std::{
    alloc::{AllocError, Allocator, Layout, System},
    num::NonZeroUsize,
    ptr::NonNull,
};

/// The global memory allocator for the SectorDB.
#[derive(Debug, Default, Copy, Clone)]
pub(crate) struct SectorAlloc;

unsafe impl Allocator for SectorAlloc {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        todo!();
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        todo!();
    }
}
