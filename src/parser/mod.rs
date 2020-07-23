use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(clippy::all)]
    pub grammar);

#[cfg(test)]
mod tests;
mod utils;
