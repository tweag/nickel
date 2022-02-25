use codespan::Files;
use nickel::environment::Environment;
use nickel::error::ParseErrors;
use nickel::eval::merge::merge;
use nickel::eval::merge::MergeMode;
use nickel::identifier::Ident;
use nickel::parser::{grammar, lexer};
use nickel::position::TermPos;
use nickel::term::BinaryOp::Merge;
use nickel::term::Contract;
use nickel::term::{BinaryOp, StrChunk};
use nickel::term::{MetaValue, RichTerm, Term};
use nickel::types::AbsType;
use nickel::types::Types;
use pretty::{BoxAllocator, DocAllocator, DocBuilder};
use std::collections::HashMap;
use std::ffi::OsString;
use std::fs;
use std::io;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, Clone)]
pub struct PrettyTerm(RichTerm);

#[derive(StructOpt, Debug)]
struct Opt {
    /// The input file
    #[structopt(name = "FILE", parse(from_os_str))]
    file: PathBuf,
}

trait NickelPretty<'a, D, A = ()>
where
    A: 'a + Clone,
    D: 'a + DocAllocator<'a, A>,
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A>;
}

impl<'a, D, A> NickelPretty<'a, D, A> for &'a MetaValue
where
    A: 'a + Clone,
    D: 'a + DocAllocator<'a, A>,
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        let MetaValue { value, types, .. } = self;
        let mut builder = allocator.nil();

        if let Some(rt) = value {
            builder = builder.append(rt.pretty(allocator));
        }

        builder = builder.append(
            allocator
                .space()
                .append(allocator.text(":"))
                .append(allocator.space()),
        );

        if let Some(Contract { types, .. }) = types {
            builder = builder.append(types.pretty(allocator));
        }

        builder
    }
}

impl<'a, D, A> NickelPretty<'a, D, A> for &'a RichTerm
where
    A: 'a + Clone,
    D: 'a + DocAllocator<'a, A>,
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        self.term.pretty(allocator)
    }
}

impl<'a, D, A> NickelPretty<'a, D, A> for &'a HashMap<Ident, RichTerm>
where
    A: 'a + Clone,
    D: 'a + DocAllocator<'a, A>,
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        //println!("{:#?}", self);
        let docs = self.iter().map(|t| t.pretty(allocator));
        let separator: DocBuilder<'a, D, A> = allocator.text(",").append(allocator.hardline());
        allocator.intersperse(docs, separator).nest(2).braces()
    }
}

impl<'a, D, A> NickelPretty<'a, D, A> for (&'a Ident, &'a RichTerm)
where
    A: 'a + Clone,
    D: 'a + DocAllocator<'a, A>,
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        let (id, term) = self;
        allocator
            .text(&id.label)
            .append(allocator.text(" = "))
            .append(term.pretty(allocator))
    }
}

impl<'a, D, A> NickelPretty<'a, D, A> for &BinaryOp
where
    A: 'a + Clone,
    D: 'a + DocAllocator<'a, A>,
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        match self {
            BinaryOp::Plus() => allocator.text(" + "),
            BinaryOp::Sub() => todo!(),
            BinaryOp::Mult() => todo!(),
            BinaryOp::Div() => todo!(),
            BinaryOp::Modulo() => todo!(),
            BinaryOp::Pow() => todo!(),
            BinaryOp::StrConcat() => todo!(),
            BinaryOp::Eq() => todo!(),
            BinaryOp::LessThan() => todo!(),
            BinaryOp::LessOrEq() => todo!(),
            BinaryOp::GreaterThan() => todo!(),
            BinaryOp::GreaterOrEq() => todo!(),
            BinaryOp::Assume() => todo!(),
            BinaryOp::Unwrap() => todo!(),
            BinaryOp::GoField() => todo!(),
            BinaryOp::Tag() => todo!(),
            BinaryOp::DynExtend() => todo!(),
            BinaryOp::DynRemove() => todo!(),
            BinaryOp::DynAccess() => todo!(),
            BinaryOp::HasField() => todo!(),
            BinaryOp::ListConcat() => todo!(),
            BinaryOp::ListElemAt() => todo!(),
            BinaryOp::Merge() => unreachable![],
            BinaryOp::Hash() => todo!(),
            BinaryOp::Serialize() => todo!(),
            BinaryOp::Deserialize() => todo!(),
            BinaryOp::StrSplit() => todo!(),
            BinaryOp::StrContains() => todo!(),
            BinaryOp::StrIsMatch() => todo!(),
            BinaryOp::StrMatch() => todo!(),
        }
    }
}

impl<'a, D, A> NickelPretty<'a, D, A> for &'a Vec<StrChunk<RichTerm>>
where
    A: 'a + Clone,
    D: 'a + DocAllocator<'a, A>,
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        let docs = self.iter().map(|chunk| chunk.pretty(allocator));
        allocator.concat(docs)
    }
}

impl<'a, D, A> NickelPretty<'a, D, A> for &'a StrChunk<RichTerm>
where
    A: 'a + Clone,
    D: 'a + DocAllocator<'a, A>,
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        match self {
            StrChunk::Literal(str) => allocator
                .text("\"")
                .append(allocator.text(str))
                .append(allocator.text("\"")),
            StrChunk::Expr(_, _) => todo!(),
        }
    }
}

impl<'a, D, A> NickelPretty<'a, D, A> for &'a Term
where
    A: 'a + Clone,
    D: 'a + DocAllocator<'a, A>,
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        match self {
            Term::Null => todo!(),
            Term::Bool(_) => todo!(),
            Term::Num(f) => allocator.as_string(f),
            Term::Str(_) => todo!(),
            Term::StrChunks(chunks) => chunks.pretty(allocator),
            Term::Fun(_, _) => todo!(),
            Term::FunPattern(None, _destruct, term) => {
                allocator.text("fun _ => ").append(term.pretty(allocator))
            }
            Term::FunPattern(Some(id), _destruct, term) => allocator
                .text("fun ")
                .append(allocator.text(&id.label))
                .append(allocator.text(" => "))
                .append(term.pretty(allocator)),
            Term::Lbl(_) => todo!(),
            Term::Let(_, _, _, _) => todo!(),
            Term::LetPattern(_, _, _, _) => todo!(),
            Term::App(_, _) => todo!(),
            Term::Var(id) => allocator.text(&id.label),
            Term::Enum(_) => todo!(),
            Term::Record(vals, attr) => {
                //let vals = vals
                //    .iter()
                //    .map(|(ident, term)| (FieldPathElem::Ident(ident), term));
                //if let Term::Record(vals, _attr) = build_record(vals, *attr) {
                vals.pretty(allocator)
                //} else {
                //    panic!()
                //}
            }
            Term::RecRecord(vals, _inter, _attr) => vals.pretty(allocator),
            Term::Switch(_, _, _) => todo!(),
            Term::List(_) => todo!(),
            Term::Op1(_, _) => todo!(),
            Term::Op2(Merge(), lhs, rhs) => {
                let env1 = Environment::new();
                let env2 = Environment::new();
                merge(
                    lhs.clone(),
                    env1,
                    rhs.clone(),
                    env2,
                    TermPos::None,
                    MergeMode::Standard,
                )
                .unwrap()
                .body
                .as_ref()
                .pretty(allocator)
            }
            Term::Op2(op, lhs, rhs) => lhs
                .pretty(allocator)
                .append(op.pretty(allocator))
                .append(rhs.pretty(allocator)),
            Term::OpN(_, _) => todo!(),
            Term::Sym(_) => todo!(),
            Term::Wrapped(_, _) => todo!(),
            Term::MetaValue(mv) => mv.pretty(allocator),
            Term::Import(_) => todo!(),
            Term::ResolvedImport(_) => todo!(),
            Term::ParseError => todo!(),
        }
    }
}

impl<'a, D, A> NickelPretty<'a, D, A> for &'a Types
where
    A: 'a + Clone,
    D: 'a + DocAllocator<'a, A>,
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
        let Types(abstype) = self;
        match abstype {
            AbsType::Dyn() => todo!(),
            AbsType::Num() => allocator.text("Num"),
            AbsType::Bool() => todo!(),
            AbsType::Str() => todo!(),
            AbsType::Sym() => todo!(),
            AbsType::Flat(_) => todo!(),
            AbsType::Arrow(_, _) => todo!(),
            AbsType::Var(_) => todo!(),
            AbsType::Forall(_, _) => todo!(),
            AbsType::RowEmpty() => todo!(),
            AbsType::RowExtend(_, _, _) => todo!(),
            AbsType::Enum(_) => todo!(),
            AbsType::StaticRecord(_) => todo!(),
            AbsType::DynRecord(_) => todo!(),
            AbsType::List(_) => todo!(),
        }
    }
}

fn main() -> io::Result<()> {
    let opt = Opt::from_args();
    let str = fs::read_to_string(&opt.file)?;
    let allocator = BoxAllocator;
    match parse(opt.file, &str) {
        Ok(t) => {
            let mut out = io::stdout();
            //println!("{:#?}", t);
            let doc: DocBuilder<_, ()> = t.pretty(&allocator);
            doc.1.render(80, &mut out)?;
        }
        Err(e) => println!("{:?}", e),
    }
    Ok(())
}

fn parse(path: impl Into<OsString>, s: &str) -> Result<RichTerm, ParseErrors> {
    let id = Files::new().add(path, s);
    grammar::TermParser::new().parse_term(id, lexer::Lexer::new(s))
}
