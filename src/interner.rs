//! A string interner.
//!
//! The primary benefits of interning strings are *deduplication* and *cheap comparison*.
//! Deduplication means allocating and storing a single instance of a string at a time.
//! Cheap comparison is achieved by using handles with small size, such as machine integers.
//!
//! # The [`Interner`] structure.
//!
//! To achive deduplication, we can use a [`HashSet`] of `Symbol` handles. Then, every time
//! we try to intern a new string, we can check 
