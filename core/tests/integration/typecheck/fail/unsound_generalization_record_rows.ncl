# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::VarLevelMismatch'
#
# [test.metadata.expectation]
# type_var = 'tail'
(
  fun r =>
    let foo = r.foo in
    let g : forall tail. { foo : _; tail } = r in
    g.baz
) : _

