# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let f | forall a. { foo: Number; a } -> Number = fun r => r.foo
in f { }