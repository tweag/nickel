# TODO for RFC007

- clarify the ownership around VM and get rid of \_with_resolver_and_table
- Use only one type of position index
- Get rid of the intermediate XXXBody
- Move the error context wrapping to higher function in the call graph (like
- `eval_closure`, so that we don't have to add context everywhere in
- `process_xary_op`)
- Review the possible risks and ergonomics of the empty-non-empty record and
- array dichotomy.
- Get rid of thunks entirely (will be part of ValueBody?)
- in `compat`, make sure we generate proper `Closurize` term prior. Is that
    really representation-efficient? Should we rather put that somewhere in the
    tag or sth?
- in cache, we should not populate everything at once during parsing. This
    required to thread a position table from the beginning, which we might not
    have, and is really annoying.
- in typechecking error, we currently cheat by returning runtime value to avoid
    handling allocators. However, this becomes hard to manage now that we need a
    position table together with values. We need to have a `Packed`
    representation of errors.

## Design notes

YAML loader borrows from the position table
