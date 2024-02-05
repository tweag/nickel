//! Pattern analysis.

use nickel_lang_core::{destructuring::*, identifier::LocIdent, term::record::Field};

pub(super) trait Bindings {
    /// Returns a list of all variables bound by this pattern, together with the path to the field they
    /// match and the associated decoration.
    ///
    /// # Example
    ///
    /// For a pattern `{a = x @ {foo, bar | Number = z}, d = e}`, the result of this function
    /// contains:
    ///
    /// - `(["a"], "foo", empty field)` for the `x` variable
    /// - `(["a", "foo"], "foo", empty field)` for the `foo` variable
    /// - `(["a", "bar"], "z", field with Number contract)` for the `z` variable
    /// - `(["d"], "e", empty field)` for the `e` variable
    fn bindings(&self) -> Vec<(Vec<LocIdent>, LocIdent, Field)>;
}

trait InjectBindings {
    /// Same as [Self::bindings], but work relative to a current path inside a pattern and injects
    /// the bindings into a working vector instead of returning the result. This method is mostly
    /// used internally and is the one performing the actual work.
    ///
    /// Other modules of the LSP should use [Bindings::bindings] directly.
    ///
    /// # Parameters
    ///
    /// - `bindings`: the vector to inject the bindings into.
    /// - `path`: the field path to the sub-pattern being analysed.
    /// - `decoration`: the decoration associated with a potential parent field pattern. For
    ///   example, when injecting the bindings of `{foo ? 5 = x @ y @ z}`, all the introduced
    ///   variables should refer to the decoration of `foo`. This decoration is thus passed along
    ///   when calling to the sub-patterns' [Bindings::inject_bindings].
    fn inject_bindings(
        &self,
        bindings: &mut Vec<(Vec<LocIdent>, LocIdent, Field)>,
        path: Vec<LocIdent>,
        parent_deco: Option<&Field>,
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

        self.pattern.inject_bindings(bindings, path, parent_deco);
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
            PatternData::RecordPattern(record_pat) => {
                record_pat.inject_bindings(bindings, path, parent_deco)
            }
        }
    }
}

impl InjectBindings for RecordPattern {
    fn inject_bindings(
        &self,
        bindings: &mut Vec<(Vec<LocIdent>, LocIdent, Field)>,
        path: Vec<LocIdent>,
        _parent_deco: Option<&Field>,
    ) {
        for field_pat in self.patterns.iter() {
            // Field patterns have their own decoration, so there's no need to propagate
            // `parent_deco` any further
            field_pat.inject_bindings(bindings, path.clone(), None);
        }
    }
}

impl InjectBindings for FieldPattern {
    fn inject_bindings(
        &self,
        bindings: &mut Vec<(Vec<LocIdent>, LocIdent, Field)>,
        mut path: Vec<LocIdent>,
        _parent_deco: Option<&Field>,
    ) {
        path.push(self.matched_id);
        self.pattern
            .inject_bindings(bindings, path, Some(&self.decoration));
    }
}
