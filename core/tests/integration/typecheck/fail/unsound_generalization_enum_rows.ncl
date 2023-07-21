# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::VarLevelMismatch'
#
# [test.metadata.expectation]
# type_var = 'tail'
(fun tag =>
  let foo = tag
    |> match { 
        _ => null,
    }
  in
  let g : forall tail. [| ; tail |] = tag in
  g
) : _
