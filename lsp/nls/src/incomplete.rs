use nickel_lang_core::{
    parser::lexer::{self, NormalToken, SpannedToken, Token},
    position::{RawPos, RawSpan},
    term::RichTerm,
};

use crate::server::Server;

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
    }

    impl DelimCounts {
        fn is_zero(&self) -> bool {
            self.paren == 0 && self.brace == 0 && self.bracket == 0 && self.enm == 0
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

pub fn parse_path_from_incomplete_input(
    range: RawSpan,
    cursor: RawPos,
    server: &mut Server,
) -> Option<RichTerm> {
    if cursor.index < range.start || cursor.index > range.end || cursor.src_id != range.src_id {
        return None;
    }

    let text = server.cache.files().source(range.src_id);
    let subtext = &text[range.start.to_usize()..cursor.index.to_usize()];

    let lexer = lexer::Lexer::new(subtext);
    let mut tokens: Vec<_> = lexer.collect::<Result<_, _>>().ok()?;

    // If the cursor is container in a token, ignore that token. If after doing that, the last
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

    let file_id = server.cache.replace_string("<incomplete-input>", to_parse);

    match server.cache.parse_nocache(file_id) {
        Ok((rt, _errors)) => Some(rt),
        Err(_) => None,
    }
}
