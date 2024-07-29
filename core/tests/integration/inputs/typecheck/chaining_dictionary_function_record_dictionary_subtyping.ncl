# test.type = 'error'
# [test.metadata]
# error = 'EvalError::BlameError'
let x = {foo = 1, bar = 2} in
let y = std.record.insert "baz" 5 x in
let _ = std.record.get "foo" y in
let z = std.record.remove "foo" y in
std.record.get "foo" z
