# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::VarLevelMismatch'
#
# [test.metadata.expectation]
# type_var = 'tail'
(
  fun tag =>
    let foo =
      tag
      |> match {
        'Foo => null,
        _ => null,
      }
    in
    let g : forall tail. [|'Foo ; tail |] = tag in
    g
) : _

