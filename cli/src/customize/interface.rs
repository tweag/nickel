//! This module defines the interface of a Nickel configuration which is used to derive the
//! customize mode of the command-line.
use super::*;

/// The interface of a configuration (a Nickel program) which represents all nested field paths
/// that are accessible from the root term together with their associated metadata.
///
/// Interface is used to derive a command-line interface from a configuration when using the
/// `customize_mode` option.
#[derive(Debug, Clone, Default)]
pub(super) struct TermInterface {
    pub(super) fields: HashMap<LocIdent, FieldInterface>,
}

/// The interface of a specific field. This field can be itself a record and contain subfields.
#[derive(Debug, Clone, Default)]
pub(super) struct FieldInterface {
    /// The interface of the subfields of this field, if it's a record itself.
    pub(super) subfields: Option<TermInterface>,
    pub(super) field: Field,
}

impl Combine for TermInterface {
    fn combine(first: Self, second: Self) -> Self {
        let TermInterface { mut fields } = first;

        for (id, field) in second.fields.into_iter() {
            if let Some(prev) = fields.remove(&id) {
                fields.insert(id, Combine::combine(prev, field));
            } else {
                fields.insert(id, field);
            }
        }

        TermInterface { fields }
    }
}

impl Combine for FieldInterface {
    fn combine(first: Self, second: Self) -> Self {
        FieldInterface {
            subfields: Combine::combine(first.subfields, second.subfields),
            field: Field {
                metadata: Combine::combine(first.field.metadata, second.field.metadata),
                // Value is used only to show a default value, and to determine if a field has a
                // definition. We don't bother actually merging the content, but just keep any
                // side that is defined.
                value: first.field.value.or(second.field.value),
                ..Default::default()
            },
        }
    }
}

impl From<&RecordData> for TermInterface {
    fn from(value: &RecordData) -> Self {
        TermInterface {
            fields: value
                .fields
                .iter()
                .map(|(id, field)| (*id, field.into()))
                .collect(),
        }
    }
}

impl From<&Term> for TermInterface {
    fn from(term: &Term) -> Self {
        term.extract_interface().unwrap_or_default()
    }
}

trait ExtractInterface {
    fn extract_interface(&self) -> Option<TermInterface>;
}

impl ExtractInterface for &Term {
    fn extract_interface(&self) -> Option<TermInterface> {
        if let Term::Record(rd) = self {
            Some(TermInterface::from(rd))
        } else {
            None
        }
    }
}

impl ExtractInterface for Field {
    fn extract_interface(&self) -> Option<TermInterface> {
        self.value.as_ref().map(|t| TermInterface::from(t.as_ref()))
    }
}

impl ExtractInterface for Type {
    fn extract_interface(&self) -> Option<TermInterface> {
        match &self.typ {
            TypeF::Record(rrows) => Some(TermInterface {
                fields: rrows
                    .iter()
                    .filter_map(|item| {
                        if let RecordRowsIteratorItem::Row(rrow) = item {
                            Some((rrow.id, FieldInterface::from(&rrow)))
                        } else {
                            None
                        }
                    })
                    .collect(),
            }),
            // Contract information is already extracted from runtime contracts, which are
            // evaluated. Here, we focus on pure static type annotations and ignore flat types as
            // well
            _ => None,
        }
    }
}

impl ExtractInterface for RuntimeContract {
    fn extract_interface(&self) -> Option<TermInterface> {
        self.contract.as_ref().extract_interface()
    }
}

impl ExtractInterface for LabeledType {
    fn extract_interface(&self) -> Option<TermInterface> {
        self.typ.extract_interface()
    }
}

impl From<&Field> for FieldInterface {
    fn from(field: &Field) -> Self {
        // We collect field information from all the sources we can: static types and contracts
        // (both either as type annotations or contract annotations), and the value of the field if
        // it's a record.

        let subfields_from_types = field
            .pending_contracts
            .iter()
            .map(ExtractInterface::extract_interface);

        let subfields_from_contracts = field
            .metadata
            .annotation
            .iter()
            .map(ExtractInterface::extract_interface);

        let subfields_from_value = std::iter::once(field.extract_interface());

        let subfields = subfields_from_types
            .chain(subfields_from_contracts)
            .chain(subfields_from_value)
            .reduce(Combine::combine)
            .flatten();

        FieldInterface {
            subfields,
            field: field.clone(),
        }
    }
}

impl From<&RecordRowF<&Type>> for FieldInterface {
    fn from(rrow: &RecordRowF<&Type>) -> Self {
        FieldInterface {
            subfields: rrow.typ.extract_interface(),
            ..Default::default()
        }
    }
}

impl FieldInterface {
    /// Define if a field is an input of a configuration that is intended to be filled. If the
    /// field is not an input, it can only be overridden via `--override`.
    ///
    /// Currently, the difference is mostly conceptual: in practice, we simply gather the arguments
    /// and their values, either via direct positional arguments or `--override` (although the
    /// latter sets a different priority), and then merge the elaborated record with original term.
    ///
    /// However, from a user experience point of view, we want to make a clear distinction between
    /// filling a bespoke input of a configuration and overriding an existing value. The latter is
    /// more "low-level" and should only be done knowingly.
    ///
    /// Currently, the logic is simply that a field is an input if it doesn't have a definition or
    /// it has a default priority. This definition might evolve, as there are ongoing discussions
    /// on what is the meaning of "input", "output", and if those concept should be made
    /// first-class ([related issue](https://github.com/tweag/nickel/issues/1505)). For now, this
    /// logic seems to be a reasonable first approximation.
    pub(super) fn is_input(&self) -> bool {
        !self.is_defined() || self.is_default()
    }

    /// Return `true` is the field has a value.
    pub(super) fn is_defined(&self) -> bool {
        self.field.value.is_some()
    }

    /// Return true is the field's merge priority is `default`.
    pub(super) fn is_default(&self) -> bool {
        matches!(self.field.metadata.priority, MergePriority::Bottom)
    }

    pub(super) fn has_subfields(&self) -> bool {
        matches!(&self.subfields, Some(ref intf) if !intf.fields.is_empty())
    }

    /// Return the list of the type and contract annotations joined as a comma-separated string, if
    /// any.
    pub(super) fn type_and_contracts(&self) -> Option<String> {
        let annotation = &self.field.metadata.annotation;

        (!annotation.is_empty()).then(|| {
            let anns: Vec<String> = annotation
                .iter()
                .map(|ctr| ctr.label.typ.to_string())
                .collect();

            anns.join(",")
        })
    }
}
