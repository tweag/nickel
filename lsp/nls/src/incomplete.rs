//! Utilities for dealing with unparseable input.
//!
//! We try to use the nickel parser when we can, but the input file is just
//! not completely parseable. Here we collect utilities for extracting some
//! information from the unparseable part. This is necessarily somewhat
//! heuristic.

use nickel_lang_core::{
    parser::lexer::{self, NormalToken, SpannedToken, Token},
    position::RawSpan,
};

use crate::world::World;

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

/// Given a range of input that we don't expect will fully parse, try to find a record access path
/// at the end of the input, parse it, properly update the usage information and the position
/// lookup table, and return the range of the parsed AST. Returns `false` if we can't find a record
/// access path or a valid sub-expression at the subrange.
///
/// For example, if the input is `let foo = bar.something.`, we will return `bar.something` (but
/// parsed and analysed for usage).
pub(crate) fn parse_incomplete_path(
    world: &mut World,
    subrange: RawSpan,
    range_err: RawSpan,
) -> bool {
    let text = world.sources.files().source(subrange.src_id);
    let subtext = &text[subrange.to_range()];

    let lexer = lexer::OffsetLexer::new(subtext, subrange.start.to_usize());
    let Ok(mut tokens) = lexer.collect::<Result<Vec<_>, _>>() else {
        return false;
    };

    // If the cursor is contained in a token, ignore that token. If after doing that, the last
    // token is a '.', ignore that one too.
    // The idea is that on the inputs `expr.a.path.foo` or `expr.a.path.`, we want to complete
    // based on the fields in `expr.a.path`
    if let Some(last) = tokens.last() {
        if last.2 == subrange.end.to_usize() {
            tokens.pop();
        }
    }
    if let Some(last) = tokens.last() {
        if let Token::Normal(NormalToken::Dot) = last.1 {
            tokens.pop();
        }
    }

    let Some(start) = path_start(&tokens) else {
        return false;
    };

    let reparse_range =
        RawSpan::from_range(subrange.src_id, tokens[start].0..tokens.last().unwrap().2);

    log::info!(
        "extracted (hopefully) reparseable input `{}`",
        &text[reparse_range.to_range()]
    );

    world.reparse_range(range_err, reparse_range)
}
