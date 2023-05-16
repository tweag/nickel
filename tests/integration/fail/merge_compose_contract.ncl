# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let DivBy = fun n l x => if x % n == 0 then x else %blame% l in
let Even = DivBy 2 in
let composed = { a | Even } & { a | DivBy 3 } in
({ a = 27 } & composed).a
