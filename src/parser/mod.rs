use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(unused_parens)]
    pub grammar);

pub mod lexer;
#[cfg(test)]
mod tests;
pub mod utils;
