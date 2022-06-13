use crate::term::make;
use crate::term::{RichTerm, Term};
use rnix::{self, SyntaxNode};

fn translate(node: &rnix::SyntaxNode) -> RichTerm {
    use rnix::SyntaxKind::*;
    println!("{:?}", node);
    match node.kind() {
        NODE_ERROR => Term::ParseError.into(),
        NODE_ROOT | NODE_PAREN => node.children().map(|n| translate(&n)).next().unwrap(),

        NODE_IDENT => Term::Var(node.text().to_string().into()).into(),
        NODE_LITERAL => node
            .children_with_tokens()
            .map(|n| {
                println!("  {:?}", n);
                match n.kind() {
                    TOKEN_INTEGER | TOKEN_FLOAT => {
                        Term::Num(n.as_token().unwrap().text().to_string().parse().unwrap()).into()
                    }
                    _ => panic!("{} token is not a literal", n),
                }
            })
            .next()
            .unwrap(),
        NODE_LET_IN => node
            .children()
            .collect::<Vec<SyntaxNode>>()
            .iter()
            .rev()
            .fold(Term::Null.into(), |rt, n| {
                if n.kind() == NODE_KEY_VALUE {
                    let key_value: Vec<SyntaxNode> = n.children().take(2).collect();
                    let key = key_value[0].first_child().unwrap().text().to_string();
                    let value = translate(key_value.get(1).unwrap());
                    make::let_rec_in(key, value, rt)
                } else {
                    translate(n)
                }
            }),
        _ => panic!("{}", node),
    }
}

pub fn parse(source: &str) -> Result<RichTerm, rnix::parser::ParseError> {
    use rnix::types::TypedNode;
    let nixast = rnix::parse(source).as_result()?;
    Ok(translate(nixast.root().node()))
}
