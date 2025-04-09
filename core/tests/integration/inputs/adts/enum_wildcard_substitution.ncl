# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'

# Check that wildcards inside enum variants are substituted for their inferred type
let f : [| 'Foo _ |] -> Number = match { 'Foo n => n } in f ('Foo "a")
