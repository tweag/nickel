# test.type = 'pass'

# regression test for https://github.com/nickel-lang/nickel/issues/2653
let Instance = { attachment_ref | { token_id | std.number.Nat, } | optional, .. } in
let raw = import "imported/nested_objects.json" in   # JSON-derived record
std.deep_seq (raw | Instance) true
