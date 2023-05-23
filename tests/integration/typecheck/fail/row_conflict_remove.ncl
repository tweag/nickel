# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::MissingRow'
#
# [test.metadata.expectation]
# ident = 'a'

# Regression test following [#144](https://github.com/tweag/nickel/issues/144). Check that
# polymorphic type variables appearing inside a row type are correctly constrained at
# instantiation.
let remove | forall c. { a : String ; c } -> { ; c } = null
in (let bad = remove (remove { a = "a" }) in 0) : Number