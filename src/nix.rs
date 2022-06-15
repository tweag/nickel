use crate::term::make;
use crate::term::{BinaryOp, UnaryOp};
use crate::term::{RichTerm, Term};
use rnix::types::{BinOp, TokenWrapper, TypedNode};
use rnix::{self, SyntaxNode};

impl From<BinOp> for RichTerm {
    fn from(op: BinOp) -> Self {
        use rnix::types::BinOpKind::*;
        let lhs = op.lhs().unwrap();
        let rhs = op.rhs().unwrap();
        match op.operator().unwrap() {
            // TODO: how to manage diff between strconcat and arrayconcat?
            Concat => Term::Op2(BinaryOp::ArrayConcat(), translate(&lhs), translate(&rhs)).into(),
            IsSet => unimplemented!(),
            Update => unimplemented!(),

            Add => Term::Op2(BinaryOp::Plus(), translate(&lhs), translate(&rhs)).into(),
            Sub => Term::Op2(BinaryOp::Sub(), translate(&lhs), translate(&rhs)).into(),
            Mul => Term::Op2(BinaryOp::Mult(), translate(&lhs), translate(&rhs)).into(),
            Div => Term::Op2(BinaryOp::Div(), translate(&lhs), translate(&rhs)).into(),

            Equal => Term::Op2(BinaryOp::Eq(), translate(&lhs), translate(&rhs)).into(),
            Less => Term::Op2(BinaryOp::LessThan(), translate(&lhs), translate(&rhs)).into(),
            More => Term::Op2(BinaryOp::GreaterThan(), translate(&lhs), translate(&rhs)).into(),
            LessOrEq => Term::Op2(BinaryOp::LessOrEq(), translate(&lhs), translate(&rhs)).into(),
            MoreOrEq => Term::Op2(BinaryOp::GreaterOrEq(), translate(&lhs), translate(&rhs)).into(),
            NotEqual => unimplemented!(),

            Implication => unimplemented!(),

            And => Term::App(
                Term::Op1(UnaryOp::BoolAnd(), translate(&lhs)).into(),
                translate(&rhs),
            )
            .into(),
            Or => Term::App(
                Term::Op1(UnaryOp::BoolOr(), translate(&lhs)).into(),
                translate(&rhs),
            )
            .into(),
        }
    }
}

fn translate(node: &rnix::SyntaxNode) -> RichTerm {
    use rnix::SyntaxKind::*;
    println!("{:?}", node);
    match node.kind() {
        NODE_ERROR => Term::ParseError.into(),
        NODE_ROOT | NODE_PAREN => node.children().map(|n| translate(&n)).next().unwrap(),

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
        NODE_STRING => {
            let parts = rnix::types::Str::cast(node.clone()).unwrap().parts();
            //TODO: Do we actualy need the `Term::Str` variant in nickel AST?
            Term::StrChunks(
                parts
                    .iter()
                    .enumerate()
                    .map(|(i, c)| match c {
                        rnix::value::StrPart::Literal(s) => {
                            crate::term::StrChunk::Literal(s.clone())
                        }
                        rnix::value::StrPart::Ast(a) => {
                            crate::term::StrChunk::Expr(translate(&a.first_child().unwrap()), i)
                        }
                    })
                    .collect(),
            )
            .into()
        }
        NODE_LIST => Term::Array(
            rnix::types::List::cast(node.clone())
                .unwrap()
                .items()
                .map(|n| translate(&n))
                .collect(),
            Default::default(),
        )
        .into(),

        NODE_IDENT => Term::Var(
            rnix::types::Ident::cast(node.clone())
                .unwrap()
                .as_str()
                .into(),
        )
        .into(),
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

        NODE_BIN_OP => BinOp::cast(node.clone()).unwrap().into(),
        _ => panic!("{}", node),
    }
}

pub fn parse(source: &str) -> Result<RichTerm, rnix::parser::ParseError> {
    use rnix::types::TypedNode;
    let nixast = rnix::parse(source).as_result()?;
    Ok(translate(nixast.root().node()))
}
