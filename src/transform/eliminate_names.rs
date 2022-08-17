//! Enrich variable terms with De Bruijn indices.
//!
//! During evaluation, we clone the environment at each let-binding or function call.
//! This means that every envrionment is a node in a tree. When resolving a variable,
//! we use the `layer` field in `VarAttrs` as an index through our view of the graph,
//! i.e a linked list. 
//!
//! ```nickel
//! let even = fun x =>
//!   let x = x % 2
//!   in x
//! in even 42
//! ```
//! The evaluator reduces a series of closures in the following fashion:
//!
//! > let even = fun x =>          |
//!     let x = x + 2              |
//!     in x                       |
//!   in even 42                   | {}
//! > even 42                      | e0 = { even = fun x => .. }
//! > even                         | e0
//! > fun x => let x = x % 2 in x  | { }
//! > let x = x % 2 in x           | e1 = { x = (42,    e0)    }
//! > x                            | e2 = { x = (x % 2, e1)    }
//! > x % 2                        | e1
//! > x                            | e1
//! > 42                           | e0
//! > 2                            | e1
//! > 0                            | {}
//! 
//! With the nameless representation, we can write the program as:
//!
//! ```nickel
//! let = fun =>
//!   let = #1 % 2
//!   in #0
//! in #0 42
//! ```
//!
//! The number following `#` represents how many layers we need to
//! traverse up the chain of envrionments in order to resolve the
//! variable. Binders either correspond to let-expressions or
//! closure expressions.
//!
//! In the above example we can see three distinct binders, each of
//! which will generate an environment during evaluation. These
//! environments generally make up a tree structure.
//!
//!         { fun => .. } <- { 42 } <- { #1 % 2 }
