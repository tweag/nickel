//! The Nickel AST, as ingested by the (future) bytecode compiler.
//!
//! Since the AST is built once for each Nickel expression and is then compiled away to bytecode,
//! the total number of allocated nodes is reasonably bounded by the input program size. Thus, for
//! performance reasons, we allocate notes using an arena and keep them alive until the end of
//! compilation. In return, we get fast allocation and de-allocation, and we can easily reference
//! other nodes and data structures using native references.
//!
//! The corresponding lifetime of all the nodes - and thus of the arena as well - is consistently
//! called `'ast`.

pub mod compat;
