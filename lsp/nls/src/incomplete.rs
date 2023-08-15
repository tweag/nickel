use nickel_lang_core::parser::lexer::{self, SpannedToken, SymbolicStringStart, Token};

fn make_static<'a>(tok: Token<'a>) -> Token<'static> {
    use nickel_lang_core::parser::lexer::NormalToken::*;

    match tok {
        Token::Normal(t) => Token::Normal(match t {
            Identifier(_) => Identifier(""),
            RawEnumTag(_) => RawEnumTag(""),
            SymbolicStringStart(s) => SymbolicStringStart(lexer::SymbolicStringStart {
                prefix: "",
                length: s.length,
            }),
            Error => Error,
            NumLiteral(n) => NumLiteral(n),
            StrEnumTagBegin => StrEnumTagBegin,
            Dyn => Dyn,
            Number => Number,
            Bool => Bool,
            String => String,
            Array => Array,
            If => If,
            Then => Then,
            Else => Else,
            Forall => Forall,
            In => In,
            Let => Let,
            Rec => Rec,
            Match => Match,
            Null => Null,
            True => True,
            False => False,
            QuestionMark => QuestionMark,
            Comma => Comma,
            Semicolon => Semicolon,
            Colon => Colon,
            Dollar => Dollar,
            Equals => Equals,
            NotEquals => NotEquals,
            Ampersand => Ampersand,
            Dot => Dot,
            DoubleQuote => DoubleQuote,
            Plus => Plus,
            Minus => Minus,
            Times => Times,
            Div => Div,
            Percent => Percent,
            DoublePlus => DoublePlus,
            DoubleEq => DoubleEq,
            At => At,
            DoubleAnd => DoubleAnd,
            DoublePipe => DoublePipe,
            Bang => Bang,
            Ellipsis => Ellipsis,
            Fun => Fun,
            Import => Import,
            Pipe => Pipe,
            RightPipe => RightPipe,
            SimpleArrow => SimpleArrow,
            DoubleArrow => DoubleArrow,
            Underscore => Underscore,
            MultiStringStart(s) => MultiStringStart(s),
            Typeof => Typeof,
            Assume => Assume,
            ArrayLazyAssume => ArrayLazyAssume,
            RecordLazyAssume => RecordLazyAssume,
            Blame => Blame,
            ChangePol => ChangePol,
            Polarity => Polarity,
            GoDom => GoDom,
            GoCodom => GoCodom,
            GoField => GoField,
            GoArray => GoArray,
            GoDict => GoDict,
            InsertTypeVar => InsertTypeVar,
            LookupTypeVar => LookupTypeVar,
            Dualize => Dualize,
            Seal => Seal,
            Unseal => Unseal,
            Embed => Embed,
            RecordMap => RecordMap,
            RecordInsert => RecordInsert,
            RecordRemove => RecordRemove,
            RecordEmptyWithTail => RecordEmptyWithTail,
            RecordSealTail => RecordSealTail,
            RecordUnsealTail => RecordUnsealTail,
            Seq => Seq,
            DeepSeq => DeepSeq,
            OpForce => OpForce,
            Length => Length,
            FieldsOf => FieldsOf,
            ValuesOf => ValuesOf,
            Pow => Pow,
            Trace => Trace,
            HasField => HasField,
            Map => Map,
            ElemAt => ElemAt,
            ArrayGen => ArrayGen,
            RecForceOp => RecForceOp,
            RecDefaultOp => RecDefaultOp,
            Merge => Merge,
            Default => Default,
            Doc => Doc,
            Optional => Optional,
            Priority => Priority,
            Force => Force,
            NotExported => NotExported,
            OpHash => OpHash,
            Serialize => Serialize,
            Deserialize => Deserialize,
            StrSplit => StrSplit,
            StrTrim => StrTrim,
            StrChars => StrChars,
            StrUppercase => StrUppercase,
            StrLowercase => StrLowercase,
            StrContains => StrContains,
            StrReplace => StrReplace,
            StrReplaceRegex => StrReplaceRegex,
            StrIsMatch => StrIsMatch,
            StrFind => StrFind,
            StrLength => StrLength,
            StrSubstr => StrSubstr,
            ToStr => ToStr,
            NumFromStr => NumFromStr,
            EnumFromStr => EnumFromStr,
            LabelWithMessage => LabelWithMessage,
            LabelWithNotes => LabelWithNotes,
            LabelAppendNote => LabelAppendNote,
            LabelPushDiag => LabelPushDiag,
            ArraySlice => ArraySlice,
            LBrace => LBrace,
            RBrace => RBrace,
            LBracket => LBracket,
            RBracket => RBracket,
            LParen => LParen,
            RParen => RParen,
            LAngleBracket => LAngleBracket,
            LessOrEq => LessOrEq,
            RAngleBracket => RAngleBracket,
            GreaterOrEq => GreaterOrEq,
            EnumOpen => EnumOpen,
            EnumClose => EnumClose,
            LineComment => LineComment,
        }),
        Token::Str(_) => todo!(),
        Token::MultiStr(_) => todo!(),
    }
}

pub struct IncompleteParser {
    lexed: Vec<SpannedToken<'static>>,
}
