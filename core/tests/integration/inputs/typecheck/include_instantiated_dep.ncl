# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::TypeMismatch'
#
# [test.metadata.expectation]
# expected = 'String'
# inferred = 'Number'

# This test checks that when adding a type annotation on a include expression,
# this annotation is used in the recursive environment instead of the original
# type assigned to the included value (even when, as it is the case here, the
# annotation is less general).
let f : forall a b. a -> b -> a = fun x _ => x in
{ 
  include f : forall b. String -> b -> String,
  x = f "hello" 42,
  y = f 42 "world",
} : _
