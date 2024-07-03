# test.type = 'error'
# [test.metadata]
# error = 'EvalError::BlameError'
let x = { foo = 1, bar =2} : {foo : Number, bar : Number} in
let y = (std.record.insert "baz" 5 x) : { _ : Number } in
let _ = (std.record.get "foo" y) : Number in
let z = (std.record.remove "foo" y) : { _ : Number } in
std.record.get "foo" z 
