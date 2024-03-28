# test.type = 'pass'
let { check, .. } = import "../lib/assert.ncl" in
let { record, .. } = std in

[
  # record.fields
  std.record.fields { a = 1, b = 2, c | optional } == ["a", "b"],
  std.record.fields ({ a = 1, b = 2 } | { a, b, c | optional }) == ["a", "b"],
  std.record.fields_all { a = 1, b = 2, c | optional } == ["a", "b", "c"],
  std.record.fields_all ({ a = 1, b = 2 } | { a, b, c | optional }) == ["a", "b", "c"],

  # record.insert
  std.record.insert "a" 1 { a | optional } == { a = 1 },

  # record.remove
  std.record.remove_all "a" { a | optional } == {},
]
|> check
