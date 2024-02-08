# test.type = 'error'
#
# [test.metadata]
# error = 'TypecheckError::UnboundTypeVariable'
#
# [test.metadata.expectation]
# identifier = 'e'
let f : forall a b c. a -> (b -> Array c) -> { foo : Array {field : a}, bar: a; e } = 
  fun bar_ _g => { foo = [{ field = bar_ }], bar = bar_ } 
in null
