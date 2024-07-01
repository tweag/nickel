# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::IncomparableValues'
let g = fun x => x + 1 in
let h = match { 0 => 0 } in
g == h
