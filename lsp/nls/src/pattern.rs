//! Pattern analysis.

use nickel_lang_core::{
    identifier::LocIdent,
    term::{pattern::*, record::Field},
};

pub(super) trait Bindings {
    /// Returns a list of all variables bound by this pattern, together with the path to the field
    /// they match and the associated extra annotations.
    ///
    /// # Example
    ///
    /// For a pattern `{a = x @ {foo, bar | Number = z, ..rest}, d = e}`, the result of this function
    /// contains:
    ///
    /// - `(["a"], "x", empty field)` for the `x` variable
    /// - `(["a"], "rest", empty field)` for the `rest` variable
    /// - `(["a", "foo"], "foo", empty field)` for the `foo` variable
    /// - `(["a", "bar"], "z", field with Number contract)` for the `z` variable
    /// - `(["d"], "e", empty field)` for the `e` variable
    fn bindings(&self) -> Vec<(Vec<LocIdent>, LocIdent, Field)>;
}

trait InjectBindings {
    /// Same as [Bindings::bindings], but work relative to a current path inside a pattern and
    /// injects the bindings into a working vector instead of returning the result. This method is
    /// mostly used internally and is the one performing the actual work.
    ///
    /// Other modules of the LSP should use [Bindings::bindings] directly.
    ///
    /// # Parameters
    ///
    /// - `bindings`: the vector to inject the bindings into.
    /// - `path`: the field path to the sub-pattern being analysed.
    /// - `parent_extra`: the extra annotations associated with a potential parent field pattern.
    ///   For example, when injecting the bindings of `{foo ? 5 = x @ y @ z}`, all the introduced
    ///   variables should refer to default annotation of `foo`. This annotation is thus passed
    ///   along when calling to the sub-patterns' [Self::inject_bindings].
    fn inject_bindings(
        &self,
        bindings: &mut Vec<(Vec<LocIdent>, LocIdent, Field)>,
        path: Vec<LocIdent>,
        parent_extra: Option<&Field>,
    );
}

impl Bindings for Pattern {
    fn bindings(&self) -> Vec<(Vec<LocIdent>, LocIdent, Field)> {
        let mut bindings = Vec::new();
        self.inject_bindings(&mut bindings, Vec::new(), None);
        bindings
    }
}

impl InjectBindings for Pattern {
    fn inject_bindings(
        &self,
        bindings: &mut Vec<(Vec<LocIdent>, LocIdent, Field)>,
        path: Vec<LocIdent>,
        parent_deco: Option<&Field>,
    ) {
        if let Some(alias) = self.alias {
            bindings.push((
                path.clone(),
                alias,
                parent_deco.cloned().unwrap_or_default(),
            ));
        }

        self.data.inject_bindings(bindings, path, parent_deco);
    }
}

impl InjectBindings for PatternData {
    fn inject_bindings(
        &self,
        bindings: &mut Vec<(Vec<LocIdent>, LocIdent, Field)>,
        path: Vec<LocIdent>,
        parent_deco: Option<&Field>,
    ) {
        match self {
            PatternData::Any(id) => {
                bindings.push((path, *id, parent_deco.cloned().unwrap_or_default()))
            }
            PatternData::Record(record_pat) => {
                record_pat.inject_bindings(bindings, path, parent_deco)
            }
            PatternData::Enum(evariant_pat) => {
                evariant_pat.inject_bindings(bindings, path, parent_deco)
            }
        }
    }
}

impl InjectBindings for RecordPattern {
    fn inject_bindings(
        &self,
        bindings: &mut Vec<(Vec<LocIdent>, LocIdent, Field)>,
        path: Vec<LocIdent>,
        parent_extra: Option<&Field>,
    ) {
        for field_pat in self.patterns.iter() {
            // Field patterns have their own annotation, so there's no need to propagate
            // `parent_extra` any further
            field_pat.inject_bindings(bindings, path.clone(), None);
        }

        if let RecordPatternTail::Capture(rest) = self.tail {
            // If a contract is attached to the whole record pattern in the enclosing pattern, the
            // rest doesn't exactly match this contract: there are some fields missing. Still, it
            // sounds more useful to keep the whole metadata - including documentation - for
            // autocompletion and the like, even if it's an over-approximation.
            bindings.push((path, rest, parent_extra.cloned().unwrap_or_default()));
        }
    }
}

impl InjectBindings for FieldPattern {
    fn inject_bindings(
        &self,
        bindings: &mut Vec<(Vec<LocIdent>, LocIdent, Field)>,
        mut path: Vec<LocIdent>,
        _parent_extra: Option<&Field>,
    ) {
        path.push(self.matched_id);
        self.pattern
            .inject_bindings(bindings, path, Some(&Field::from(self.annotation.clone())));
    }
}

impl InjectBindings for EnumPattern {
    fn inject_bindings(
        &self,
        bindings: &mut Vec<(Vec<LocIdent>, LocIdent, Field)>,
        path: Vec<LocIdent>,
        _parent_extra: Option<&Field>,
    ) {
        //TODO: I'm not sure we should just transparently forward to the variant's argument. Maybe
        //we need a more complex notion of path here, that knows when we enter an enum variant?
        if let Some(ref arg_pat) = self.pattern {
            arg_pat.inject_bindings(bindings, path, None);
        }
    }
}
