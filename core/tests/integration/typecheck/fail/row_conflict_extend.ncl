# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::RecordRowConflict'
#
# [test.metadata.expectation]
# row = 'a'

# Regression test following [#144](https://github.com/tweag/nickel/issues/144). Check that
# polymorphic type variables appearing inside a row type are correctly constrained at
# instantiation.
let extend | forall c. { ; c } -> { a : String ; c} = null 
in (let bad = extend { a  = 1 } in 0) : Number
