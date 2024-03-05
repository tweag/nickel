# test.type = 'error'
# eval = 'typecheck'
#
# [test.metadata]
# error = 'TypecheckError::EnumRowConflict'
#
# [test.metadata.expectation]
# row = 'Foo'
let f
  | forall r. [| ; r |] -> [| 'Foo Number, 'Bar Number ; r |]
  # the implementation doesn't matter, we care about types
  = fun r => null
  in
let g = fun x => 'a in
g ((f ('Foo "a")) : _)

