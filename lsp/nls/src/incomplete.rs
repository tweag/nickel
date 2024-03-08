//! Utilities for dealing with unparseable input.
//!
//! We try to use the nickel parser when we can, but the input file is just
//! not completely parseable. Here we collect utilities for extracting some
//! information from the unparseable part. This is necessarily somewhat
//! heuristic.

use std::path::PathBuf;

use nickel_lang_core::{
    cache::SourcePath,
    parser::lexer::{self, NormalToken, SpannedToken, Token},
    position::RawSpan,
    term::{RichTerm, Term},
    transform::import_resolution,
};

use crate::{usage::Environment, world::World};

// Take a bunch of tokens and the end of a possibly-delimited sequence, and return the
// index of the beginning of the possibly-delimited sequence. The sequence might not
// actually be delimited, which counts as a sequence of length 1.
//
// This is somewhat overly-permissive as it doesn't ensure that delimiters are
// correctly nested. For example, it won't report any errors on '[(])'.
fn delimited_start(toks: &[SpannedToken], mut idx: usize) -> Option<usize> {
    #[derive(Default)]
    struct DelimCounts {
        paren: i32,
        brace: i32,
        bracket: i32,
        enm: i32,
        double_quote: bool,
    }

    impl DelimCounts {
        fn is_zero(&self) -> bool {
            self.paren == 0
                && self.brace == 0
                && self.bracket == 0
                && self.enm == 0
                && !self.double_quote
        }

        fn is_err(&self) -> bool {
            self.paren < 0 || self.brace < 0 || self.bracket < 0 || self.enm < 0
        }

        fn update(&mut self, tok: &Token) {
            match tok {
                // The signs look wrong here, but it's because we read through the tokens
                // right-to-left.
                Token::Normal(NormalToken::LBrace) => self.brace -= 1,
                Token::Normal(NormalToken::RBrace) => self.brace += 1,
                Token::Normal(NormalToken::LParen) => self.paren -= 1,
                Token::Normal(NormalToken::RParen) => self.paren += 1,
                Token::Normal(NormalToken::LBracket) => self.bracket -= 1,
                Token::Normal(NormalToken::RBracket) => self.bracket += 1,
                Token::Normal(NormalToken::EnumOpen) => self.enm -= 1,
                Token::Normal(NormalToken::EnumClose) => self.enm += 1,
                Token::Normal(NormalToken::DoubleQuote) => self.double_quote = !self.double_quote,
                _ => {}
            }
        }
    }

    let mut counts = DelimCounts::default();
    loop {
        counts.update(&toks[idx].1);
        if counts.is_zero() {
            return Some(idx);
        } else if counts.is_err() || idx == 0 {
            return None;
        }

        idx -= 1;
    }
}

// Given tokens that hopefully end in a record path, return the index of the
// token that starts the record path.
fn path_start(toks: &[SpannedToken]) -> Option<usize> {
    let mut idx = toks.len().checked_sub(1)?;

    loop {
        idx = delimited_start(toks, idx)?;

        if idx == 0 || toks[idx - 1].1 != Token::Normal(NormalToken::Dot) {
            return Some(idx);
        }

        // Skip over the '.' to the end of the preceding token.
        idx = idx.checked_sub(2)?;
    }
}

fn resolve_imports(rt: RichTerm, world: &mut World) -> RichTerm {
    let import_resolution::tolerant::ResolveResult {
        transformed_term,
        resolved_ids,
        ..
    } = import_resolution::tolerant::resolve_imports(rt, &mut world.cache);

    for id in resolved_ids {
        if world.cache.parse(id).is_ok() {
            // If a new input got imported in an incomplete term, try to typecheck
            // (and build lookup tables etc.) for it, but don't issue diagnostics.
            let _ = world.typecheck(id);
        }
    }

    transformed_term
}

/// Given a range of input that we don't expect will fully parse, try to find
/// a record access path at the end of the input, parse it, and return the
/// resulting term.
///
/// For example, if the input is `let foo = bar.something.`, we will return
/// `bar.something` (but parsed, of course).
pub fn parse_path_from_incomplete_input(
    range: RawSpan,
    env: &Environment,
    world: &mut World,
) -> Option<RichTerm> {
    let text = world.cache.files().source(range.src_id);
    let subtext = &text[range.start.to_usize()..range.end.to_usize()];

    let lexer = lexer::Lexer::new(subtext);
    let mut tokens: Vec<_> = lexer.collect::<Result<_, _>>().ok()?;

    // If the cursor is contained in a token, ignore that token. If after doing that, the last
    // token is a '.', ignore that one too.
    // The idea is that on the inputs `expr.a.path.foo` or `expr.a.path.`, we want to complete
    // based on the fields in `expr.a.path`
    if let Some(last) = tokens.last() {
        if last.2 >= subtext.len() {
            tokens.pop();
        }
    }
    if let Some(last) = tokens.last() {
        if last.1 == Token::Normal(NormalToken::Dot) {
            tokens.pop();
        }
    }

    let start = path_start(&tokens)?;
    let to_parse = subtext[tokens[start].0..tokens.last().unwrap().2].to_owned();
    log::info!("extracted (hopefully) reparseable input `{to_parse}`");

    // In order to help the input resolver find relative imports, we add a fake input whose parent
    // is the same as the real file.
    let path = PathBuf::from(world.cache.files().name(range.src_id));
    let file_id = world
        .cache
        .replace_string(SourcePath::Snippet(path), to_parse);

    match world.cache.parse_nocache(file_id) {
        Ok((rt, _errors)) if !matches!(rt.as_ref(), Term::ParseError(_)) => {
            world.analysis.insert_usage(file_id, &rt, env);
            Some(resolve_imports(rt, world))
        }
        _ => None,
    }
}
