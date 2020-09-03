use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(unused_parens)]
    pub grammar);

#[cfg(test)]
mod tests;
pub mod utils;
