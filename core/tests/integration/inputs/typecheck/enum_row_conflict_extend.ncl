# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::EnumRowConflict'
#
# [test.metadata.expectation]
# row = 'Foo'

# Regression test following [#144](https://github.com/tweag/nickel/issues/144). Check that
# polymorphic type variables appearing inside a row type are correctly constrained at
# instantiation.
let extend | forall c. [| ; c |] -> [| 'Foo String ; c |] = null 
in (let bad = extend ('Foo "a") in 0) : Number
