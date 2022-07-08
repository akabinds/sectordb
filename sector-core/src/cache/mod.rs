//! Caching algorithm and implementation for ModDB.
//!
//! A database cache stores the results of commonly-used queries to prevent having to execute expensive queries each time their result is needed.
//! This reduces excessive and unwanted strain on the database.

#[cfg(test)]
use pretty_assertions::assert_eq;
