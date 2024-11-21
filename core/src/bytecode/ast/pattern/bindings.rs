//! Pattern analysis. The trait defined here is mostly used by the LSP.

use crate::{
    bytecode::ast::{pattern::*, record::FieldMetadata},
    identifier::LocIdent,
};

/// A variable bound in a pattern, together with the path to the field it matches and the
/// associated extra annotations.
pub struct PatBinding<'ast> {
    pub path: Vec<LocIdent>,
    pub id: LocIdent,
    pub metadata: FieldMetadata<'ast>,
}

pub trait Bindings<'ast> {
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
    fn bindings(&self) -> Vec<PatBinding<'ast>>;
}

trait InjectBindings<'ast> {
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
        bindings: &mut Vec<PatBinding<'ast>>,
        path: Vec<LocIdent>,
        parent_extra: Option<&FieldMetadata<'ast>>,
    );
}

impl<'ast> Bindings<'ast> for Pattern<'ast> {
    fn bindings(&self) -> Vec<PatBinding<'ast>> {
        let mut bindings = Vec::new();
        self.inject_bindings(&mut bindings, Vec::new(), None);
        bindings
    }
}

impl<'ast> InjectBindings<'ast> for Pattern<'ast> {
    fn inject_bindings(
        &self,
        bindings: &mut Vec<PatBinding<'ast>>,
        path: Vec<LocIdent>,
        parent_deco: Option<&FieldMetadata<'ast>>,
    ) {
        if let Some(alias) = self.alias {
            bindings.push(PatBinding {
                path: path.clone(),
                id: alias,
                metadata: parent_deco.cloned().unwrap_or_default(),
            });
        }

        self.data.inject_bindings(bindings, path, parent_deco);
    }
}

impl<'ast> InjectBindings<'ast> for PatternData<'ast> {
    fn inject_bindings(
        &self,
        bindings: &mut Vec<PatBinding<'ast>>,
        path: Vec<LocIdent>,
        parent_deco: Option<&FieldMetadata<'ast>>,
    ) {
        match self {
            PatternData::Any(id) => bindings.push(PatBinding {
                path,
                id: *id,
                metadata: parent_deco.cloned().unwrap_or_default(),
            }),
            PatternData::Record(record_pat) => {
                record_pat.inject_bindings(bindings, path, parent_deco)
            }
            PatternData::Array(array_pat) => array_pat.inject_bindings(bindings, path, parent_deco),
            PatternData::Enum(evariant_pat) => {
                evariant_pat.inject_bindings(bindings, path, parent_deco)
            }
            PatternData::Or(or_pat) => or_pat.inject_bindings(bindings, path, parent_deco),
            // Wildcard and constant patterns don't bind any variable
            PatternData::Wildcard | PatternData::Constant(_) => (),
        }
    }
}

impl<'ast> InjectBindings<'ast> for RecordPattern<'ast> {
    fn inject_bindings(
        &self,
        bindings: &mut Vec<PatBinding<'ast>>,
        path: Vec<LocIdent>,
        parent_extra: Option<&FieldMetadata<'ast>>,
    ) {
        for field_pat in self.patterns.iter() {
            // Field patterns have their own annotation, so there's no need to propagate
            // `parent_extra` any further
            field_pat.inject_bindings(bindings, path.clone(), None);
        }

        if let TailPattern::Capture(rest) = self.tail {
            // If a contract is attached to the whole record pattern in the enclosing pattern, the
            // rest doesn't exactly match this contract: there are some fields missing. Still, it
            // sounds more useful to keep the whole metadata - including documentation - for
            // autocompletion and the like, even if it's an over-approximation.
            bindings.push(PatBinding {
                path,
                id: rest,
                metadata: parent_extra.cloned().unwrap_or_default(),
            });
        }
    }
}

impl<'ast> InjectBindings<'ast> for ArrayPattern<'ast> {
    fn inject_bindings(
        &self,
        bindings: &mut Vec<PatBinding<'ast>>,
        path: Vec<LocIdent>,
        _parent_extra: Option<&FieldMetadata<'ast>>,
    ) {
        for subpat in self.patterns.iter() {
            // Array elements shouldn't inherit the annotation from their parent (for once, they
            // are of a different type), so we reset `parent_extra` to `None`.
            subpat.inject_bindings(bindings, path.clone(), None);
        }

        if let TailPattern::Capture(rest) = self.tail {
            bindings.push(PatBinding {
                path,
                id: rest,
                metadata: Default::default(),
            });
        }
    }
}

impl<'ast> InjectBindings<'ast> for FieldPattern<'ast> {
    fn inject_bindings(
        &self,
        bindings: &mut Vec<PatBinding<'ast>>,
        mut path: Vec<LocIdent>,
        _parent_extra: Option<&FieldMetadata<'ast>>,
    ) {
        path.push(self.matched_id);
        self.pattern.inject_bindings(
            bindings,
            path,
            Some(&FieldMetadata::from(self.annotation.clone())),
        );
    }
}

impl<'ast> InjectBindings<'ast> for EnumPattern<'ast> {
    fn inject_bindings(
        &self,
        bindings: &mut Vec<PatBinding<'ast>>,
        path: Vec<LocIdent>,
        _parent_extra: Option<&FieldMetadata<'ast>>,
    ) {
        //TODO: I'm not sure we should just transparently forward to the variant's argument. Maybe
        //we need a more complex notion of path here, that knows when we enter an enum variant?
        if let Some(ref arg_pat) = self.pattern {
            arg_pat.inject_bindings(bindings, path, None);
        }
    }
}

impl<'ast> InjectBindings<'ast> for OrPattern<'ast> {
    fn inject_bindings(
        &self,
        bindings: &mut Vec<PatBinding<'ast>>,
        path: Vec<LocIdent>,
        parent_extra: Option<&FieldMetadata<'ast>>,
    ) {
        for subpat in self.patterns.iter() {
            subpat.inject_bindings(bindings, path.clone(), parent_extra);
        }
    }
}
