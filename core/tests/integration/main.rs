use std::{io::Cursor, thread};

use nickel_lang_core::{
    error::{
        Error, EvalError, ExportError, ExportErrorData, ImportError, ParseError, TypecheckError,
    },
    term::Term,
};
use nickel_lang_utils::{
    annotated_test::{read_annotated_test_case, TestCase},
    project_root::project_root,
    test_program::TestProgram,
};
use serde::Deserialize;
use test_generator::test_resources;

mod contract_label_path;
mod free_vars;
mod pretty;
mod query;
mod stdlib_typecheck;

#[test_resources("core/tests/integration/**/*.ncl")]
fn check_annotated_nickel_file(path: &str) {
    let test: TestCase<Test> =
        read_annotated_test_case(path).expect("Failed to parse annotated program");

    // By default, cargo runs tests with a 2MB stack, which we can overflow in
    // debug mode. To avoid this we run the tests with an increased stack size.
    const STACK_SIZE: usize = 4 * 1024 * 1024;
    let path = String::from(project_root().join(path).to_string_lossy());

    thread::Builder::new()
        .name(path.clone())
        .stack_size(STACK_SIZE)
        .spawn(move || run_test(test, path))
        .expect("Failed to spawn thread")
        .join()
        .expect("Failed to join thread")
}

// Like check_annotated_nickel_file, but runs the test from the directory of
// the test file itself (and opens the test file with a relative path). This
// is mainly for integration testing path normalization.
#[test_resources("core/tests/integration/inputs/imports/imported/import_parent.ncl")]
fn check_from_dir(path: &str) {
    let test: TestCase<Test> =
        read_annotated_test_case(path).expect("Failed to parse annotated program");

    let path = project_root().join(path);
    let dir = std::env::current_dir().unwrap();
    let test_dir = path.parent().unwrap();
    std::env::set_current_dir(test_dir).unwrap();
    run_test(
        test,
        String::from(path.file_name().unwrap().to_string_lossy()),
    );
    std::env::set_current_dir(dir).unwrap();
}

fn run_test(test_case: TestCase<Test>, path: String) {
    let repeat = test_case.annotation.repeat.unwrap_or(1);
    let eval_strategy = test_case.annotation.eval.unwrap_or(EvalStrategy::Standard);
    let program = test_case.program;
    let test = test_case.annotation.test;

    for _ in 0..repeat {
        let mut p = TestProgram::new_from_source(
            Cursor::new(program.clone()),
            path.as_str(),
            std::io::stderr(),
        )
        .expect("");
        if let Some(imports) = &test_case.annotation.nickel_path {
            p.add_import_paths(imports.iter());
        }
        match test.clone() {
            Expectation::Error(expected_err) => {
                let err = eval_strategy.eval_program_to_err(p);
                assert_eq!(expected_err, err, "wrong error evaluating file {path}")
            }
            Expectation::Pass => {
                let result = eval_strategy.eval_program_to_term(p);
                assert_eq!(
                    result,
                    Term::Bool(true),
                    "unexpected error evaluating file {path}",
                )
            }
            Expectation::Skip => (),
        }
    }
}

#[derive(Deserialize)]
struct Test {
    test: Expectation,
    repeat: Option<usize>,
    eval: Option<EvalStrategy>,
    nickel_path: Option<Vec<String>>,
}

#[derive(Clone, Copy, Deserialize)]
enum EvalStrategy {
    #[serde(rename = "full")]
    Full,
    #[serde(rename = "standard")]
    Standard,
    #[serde(rename = "typecheck")]
    TypeCheck,
}

impl EvalStrategy {
    fn eval_program_to_term(&self, mut p: TestProgram) -> Term {
        match self {
            EvalStrategy::Full => p.eval_full().map(Term::from),
            EvalStrategy::Standard => p.eval().map(Term::from),
            EvalStrategy::TypeCheck => p.typecheck().map(|_| Term::Bool(true)),
        }
        .expect("Expected evaluation to succeed but got an error")
    }

    fn eval_program_to_err(&self, mut p: TestProgram) -> Error {
        match self {
            EvalStrategy::Full => p.eval_full().map(|_| ()),
            EvalStrategy::Standard => p.eval().map(|_| ()),
            EvalStrategy::TypeCheck => p.typecheck(),
        }
        .expect_err("Expected an error but program evaluated successfully")
    }
}

#[derive(Clone, Deserialize)]
#[serde(tag = "type", content = "metadata")]
enum Expectation {
    #[serde(rename = "error")]
    Error(ErrorExpectation),
    #[serde(rename = "pass")]
    Pass,
    #[serde(rename = "skip")]
    Skip,
}

#[derive(Clone, Deserialize)]
#[serde(tag = "error", content = "expectation")]
enum ErrorExpectation {
    // TODO: can we somehow unify this with the `Display` impl below?
    #[serde(rename = "EvalError::EqError")]
    EvalEqError,
    #[serde(rename = "EvalError::Other")]
    EvalOther,
    #[serde(rename = "EvalError::UnaryPrimopTypeError")]
    EvalUnaryPrimopTypeError,
    #[serde(rename = "EvalError::NAryPrimopTypeError")]
    EvalNAryPrimopTypeError,
    #[serde(rename = "EvalError::BlameError")]
    EvalBlameError,
    #[serde(rename = "EvalError::IllegalPolymorphicTailAccess")]
    EvalIllegalPolymorphicTailAccess,
    #[serde(rename = "EvalError::TypeError")]
    EvalTypeError,
    #[serde(rename = "EvalError::InfiniteRecursion")]
    EvalInfiniteRecursion,
    #[serde(rename = "EvalError::FieldMissing")]
    EvalFieldMissing { field: String },
    #[serde(rename = "EvalError::MissingFieldDef")]
    EvalMissingFieldDef { field: String },
    #[serde(rename = "EvalError::MergeIncompatibleArgs")]
    EvalMergeIncompatibleArgs,
    #[serde(rename = "EvalError::NonExhaustiveMatch")]
    EvalNonExhaustiveMatch,
    #[serde(rename = "EvalError::NonExhaustiveEnumMatch")]
    EvalNonExhaustiveEnumMatch,
    #[serde(rename = "TypecheckError::UnboundIdentifier")]
    TypecheckUnboundIdentifier { identifier: String },
    #[serde(rename = "TypecheckError::UnboundTypeVariable")]
    TypecheckUnboundTypeVariable { identifier: String },
    #[serde(rename = "TypecheckError::TypeMismatch")]
    TypecheckTypeMismatch { expected: String, inferred: String },
    #[serde(rename = "TypecheckError::ForallParametricityViolation")]
    TypecheckForallParametricityViolation {
        tail: String,
        violating_type: String,
    },
    #[serde(rename = "TypecheckError::MissingRow")]
    TypecheckMissingRow { ident: String },
    #[serde(rename = "TypecheckError::ExtraRow")]
    TypecheckExtraRow { ident: String },
    #[serde(rename = "TypecheckError::RecordRowConflict")]
    TypecheckRecordRowConflict { row: String },
    #[serde(rename = "TypecheckError::EnumRowConflict")]
    TypecheckEnumRowConflict { row: String },
    #[serde(rename = "TypecheckError::RecordRowMismatch")]
    TypecheckRecordRowMismatch,
    #[serde(rename = "TypecheckError::EnumRowMismatch")]
    TypecheckEnumRowMismatch,
    #[serde(rename = "TypecheckError::ExtraDynTail")]
    TypecheckExtraDynTail,
    #[serde(rename = "TypecheckError::MissingDynTail")]
    TypecheckMissingDynTail,
    #[serde(rename = "TypecheckError::ArrowTypeMismatch")]
    TypecheckArrowTypeMismatch { cause: Box<ErrorExpectation> },
    #[serde(rename = "TypecheckError::FlatTypeInTermPosition")]
    TypecheckFlatTypeInTermPosition,
    #[serde(rename = "TypecheckError::VarLevelMismatch")]
    TypecheckVarLevelMismatch { type_var: String },
    #[serde(rename = "TypecheckError::OrPatternVarsMismatch")]
    TypecheckOrPatternVarsMismatch { var: String },
    #[serde(rename = "ParseError")]
    AnyParseError,
    #[serde(rename = "ParseError::DuplicateIdentInRecordPattern")]
    ParseDuplicateIdentInRecordPattern { ident: String },
    #[serde(rename = "ParseError::TypedFieldWithoutDefinition")]
    ParseTypedFieldWithoutDefinition,
    #[serde(rename = "ImportError::ParseError")]
    ImportParseError,
    #[serde(rename = "ImportError::IoError")]
    ImportIoError,
    #[serde(rename = "ExportError::NumberOutOfRange")]
    SerializeNumberOutOfRange,
}

impl PartialEq<Error> for ErrorExpectation {
    fn eq(&self, other: &Error) -> bool {
        use ErrorExpectation::*;
        match (self, other) {
            (EvalBlameError, Error::EvalError(EvalError::BlameError { .. }))
            | (
                EvalIllegalPolymorphicTailAccess,
                Error::EvalError(EvalError::IllegalPolymorphicTailAccess { .. }),
            )
            | (EvalTypeError, Error::EvalError(EvalError::TypeError(..)))
            | (EvalEqError, Error::EvalError(EvalError::EqError { .. }))
            | (EvalNAryPrimopTypeError, Error::EvalError(EvalError::NAryPrimopTypeError { .. }))
            | (
                EvalUnaryPrimopTypeError,
                Error::EvalError(EvalError::UnaryPrimopTypeError { .. }),
            )
            | (EvalInfiniteRecursion, Error::EvalError(EvalError::InfiniteRecursion(..)))
            | (
                EvalMergeIncompatibleArgs,
                Error::EvalError(EvalError::MergeIncompatibleArgs { .. }),
            )
            | (EvalOther, Error::EvalError(EvalError::Other(..)))
            | (EvalNonExhaustiveMatch, Error::EvalError(EvalError::NonExhaustiveMatch { .. }))
            | (
                EvalNonExhaustiveEnumMatch,
                Error::EvalError(EvalError::NonExhaustiveEnumMatch { .. }),
            )
            | (
                TypecheckRecordRowMismatch,
                Error::TypecheckError(TypecheckError::RecordRowMismatch { .. }),
            )
            | (
                TypecheckEnumRowMismatch,
                Error::TypecheckError(TypecheckError::EnumRowMismatch { .. }),
            )
            | (
                TypecheckMissingDynTail,
                Error::TypecheckError(TypecheckError::MissingDynTail { .. }),
            )
            | (TypecheckExtraDynTail, Error::TypecheckError(TypecheckError::ExtraDynTail { .. }))
            | (
                TypecheckFlatTypeInTermPosition,
                Error::TypecheckError(TypecheckError::FlatTypeInTermPosition { .. }),
            )
            | (ImportParseError, Error::ImportError(ImportError::ParseErrors(..)))
            | (ImportIoError, Error::ImportError(ImportError::IOError(..)))
            | (
                SerializeNumberOutOfRange,
                Error::EvalError(EvalError::SerializationError(ExportError {
                    data: ExportErrorData::NumberOutOfRange { .. },
                    ..
                })),
            ) => true,
            (e, Error::ParseErrors(es)) => {
                let first_error = es
                    .errors
                    .first()
                    .expect("Got ParserErrors without any errors");
                match (e, first_error) {
                    (AnyParseError, _) => true,
                    (
                        ParseDuplicateIdentInRecordPattern { ident },
                        ParseError::DuplicateIdentInRecordPattern { ident: ident1, .. },
                    ) => ident.as_str() == ident1.label(),
                    (
                        ParseTypedFieldWithoutDefinition,
                        ParseError::TypedFieldWithoutDefinition { .. },
                    ) => true,
                    _ => false,
                }
            }
            (
                EvalFieldMissing { field },
                Error::EvalError(EvalError::FieldMissing { id: name, .. }),
            ) => field == name.label(),
            (
                EvalMissingFieldDef { field },
                Error::EvalError(EvalError::MissingFieldDef { id, .. }),
            ) => field == id.label(),
            (
                TypecheckUnboundIdentifier { identifier },
                Error::TypecheckError(TypecheckError::UnboundIdentifier { id, .. }),
            ) => id.label() == identifier,
            (
                TypecheckUnboundTypeVariable { identifier },
                Error::TypecheckError(TypecheckError::UnboundTypeVariable(ident)),
            ) => identifier == ident.label(),
            (
                TypecheckTypeMismatch { expected, inferred },
                Error::TypecheckError(
                    TypecheckError::TypeMismatch {
                        expected: expected1,
                        inferred: inferred1,
                        ..
                    }
                    | TypecheckError::ArrowTypeMismatch {
                        expected: expected1,
                        inferred: inferred1,
                        ..
                    },
                ),
            ) if expected == &expected1.to_string() && inferred == &inferred1.to_string() => true,
            (
                TypecheckForallParametricityViolation {
                    tail,
                    violating_type,
                },
                Error::TypecheckError(TypecheckError::ForallParametricityViolation {
                    tail: tail1,
                    violating_type: vtype1,
                    ..
                }),
            ) => {
                tail.as_str() == tail1.to_string() && violating_type.as_str() == vtype1.to_string()
            }
            (
                TypecheckMissingRow { ident },
                Error::TypecheckError(TypecheckError::MissingRow { id: id1, .. }),
            ) if ident == id1.label() => true,
            (
                TypecheckExtraRow { ident },
                Error::TypecheckError(TypecheckError::ExtraRow { id: id1, .. }),
            ) if ident == id1.label() => true,
            (
                TypecheckRecordRowConflict { row },
                Error::TypecheckError(TypecheckError::RecordRowConflict { row: row1, .. }),
            ) => row == row1.id.label(),
            (
                TypecheckEnumRowConflict { row },
                Error::TypecheckError(TypecheckError::EnumRowConflict { row: row1, .. }),
            ) => row == row1.id.label(),
            (
                TypecheckVarLevelMismatch { type_var: ident },
                Error::TypecheckError(TypecheckError::VarLevelMismatch {
                    type_var: constant, ..
                }),
            ) => ident == constant.label(),
            (
                TypecheckOrPatternVarsMismatch { var },
                Error::TypecheckError(TypecheckError::OrPatternVarsMismatch { var: id, .. }),
            ) => var == id.label(),
            // The clone is not ideal, but currently we can't compare `TypecheckError` directly
            // with an ErrorExpectation. Ideally, we would implement `eq` for all error subtypes,
            // and have the eq with `Error` just dispatch to those sub-eq functions.
            (
                TypecheckArrowTypeMismatch { cause },
                Error::TypecheckError(TypecheckError::ArrowTypeMismatch { cause: cause2, .. }),
            ) => cause.as_ref() == &Error::TypecheckError((**cause2).clone()),
            // If nothing else matched up to this point, we allow the expected error to appear wrapped inside an `ArrowTypeMismatch`
            //
            (error_exp, Error::TypecheckError(TypecheckError::ArrowTypeMismatch { cause, .. })) => {
                error_exp == &Error::TypecheckError((**cause).clone())
            }
            (_, _) => false,
        }
    }
}

impl std::fmt::Display for ErrorExpectation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ErrorExpectation::*;
        let name = match self {
            AnyParseError => "ParseError".to_owned(),
            ParseDuplicateIdentInRecordPattern { ident } => {
                format!("ParseError::DuplicateIdentInRecordPattern({ident})")
            }
            ParseTypedFieldWithoutDefinition => {
                "ParseError::TypedFieldWithoutDefinition".to_owned()
            }
            ImportParseError => "ImportError::ParseError".to_owned(),
            ImportIoError => "ImportError::IoError".to_owned(),
            EvalBlameError => "EvalError::BlameError".to_owned(),
            EvalTypeError => "EvalError::TypeError".to_owned(),
            EvalEqError => "EvalError::EqError".to_owned(),
            EvalOther => "EvalError::Other".to_owned(),
            EvalMergeIncompatibleArgs => "EvalError::MergeIncompatibleArgs".to_owned(),
            EvalNAryPrimopTypeError => "EvalError::NAryPrimopTypeError".to_owned(),
            EvalUnaryPrimopTypeError => "EvalError::UnaryPrimopTypeError".to_owned(),
            EvalInfiniteRecursion => "EvalError::InfiniteRecursion".to_owned(),
            EvalIllegalPolymorphicTailAccess => {
                "EvalError::IllegalPolymorphicTailAccess".to_owned()
            }
            EvalFieldMissing { field } => {
                format!("EvalError::FieldMissing({field})")
            }
            EvalMissingFieldDef { field } => {
                format!("EvalError::MissingFieldDef({field})")
            }
            EvalNonExhaustiveMatch => "EvalError::NonExhaustiveMatch".to_owned(),
            EvalNonExhaustiveEnumMatch => "EvalError::NonExhaustiveEnumMatch".to_owned(),
            TypecheckUnboundIdentifier { identifier } => {
                format!("TypecheckError::UnboundIdentifier({identifier})")
            }
            TypecheckUnboundTypeVariable { identifier } => {
                format!("TypecheckError::UnboundTypeVariable({identifier})")
            }
            TypecheckTypeMismatch { expected, inferred } => {
                format!("TypecheckError::TypeMismatch({expected}, {inferred})")
            }
            TypecheckForallParametricityViolation {
                tail,
                violating_type,
            } => {
                format!("TypecheckError::ForallParametricityViolation({tail}, {violating_type})")
            }
            TypecheckMissingRow { ident } => {
                format!("TypecheckError::MissingRow({ident})")
            }
            TypecheckExtraRow { ident } => {
                format!("TypecheckError::ExtraRow({ident})")
            }
            TypecheckRecordRowMismatch => "TypecheckError::RecordRowMismatch".to_owned(),
            TypecheckEnumRowMismatch => "TypecheckError::EnumRowMismatch".to_owned(),
            TypecheckRecordRowConflict { row } => {
                format!("TypecheckError::RecordRowConflict({row})")
            }
            TypecheckEnumRowConflict { row } => {
                format!("TypecheckError::EnumRowConflict({row})")
            }
            TypecheckExtraDynTail => "TypecheckError::ExtraDynTail".to_owned(),
            TypecheckMissingDynTail => "TypecheckError::MissingDynTail".to_owned(),
            TypecheckArrowTypeMismatch { cause } => {
                format!("TypecheckError::ArrowTypeMismatch({cause})")
            }
            TypecheckFlatTypeInTermPosition => "TypecheckError::FlatTypeInTermPosition".to_owned(),
            TypecheckVarLevelMismatch { type_var } => {
                format!("TypecheckError::VarLevelMismatch({type_var})")
            }
            TypecheckOrPatternVarsMismatch { var } => {
                format!("TypecheckError::OrPatternVarsMismatch({var})")
            }
            SerializeNumberOutOfRange => "ExportError::NumberOutOfRange".to_owned(),
        };
        write!(f, "{}", name)
    }
}

impl std::fmt::Debug for ErrorExpectation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
