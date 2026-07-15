//! Versioned wire adapters for semantic simulator traces.

mod trace;
mod wire;

pub use trace::*;
pub use wire::*;

pub const TRACE_SCHEMA_VERSION: u16 = 1;
