# test.type = 'error'
#
# [test.metadata]
# error = 'TypecheckError::UnboundIdentifier'
#
# [test.metadata.expectation]
# identifier = 'd'
let f | forall a b c. a -> (b -> Array c) -> {foo : Array {_ : d}, bar: a; Dyn}
  = fun bar_ _g => {foo = [{field = 1}], bar = bar_} 
in (f 1 (fun _x => [])).foo == [{ field = 1 }]