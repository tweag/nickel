use crate::term::RichTerm;

pub fn transform_one(rt: RichTerm) -> RichTerm {
    use crate::term::Term::RecRecord;
    if let RecRecord(record, dyn_fields, deps, inh) = *rt.term {
    } else {
        rt
    }
}
