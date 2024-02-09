pub mod completion;
pub mod goto;
pub mod hover;
pub mod rename;
pub mod symbols;

#[cfg(feature = "format")]
pub mod formatting;

#[cfg(not(feature = "format"))]
#[path = "formatting_external.rs"]
pub mod formatting;
