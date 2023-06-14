# test.type = 'error'
# eval = 'typecheck'
# 
# [test.metadata]
# error = 'TypecheckError::MissingRow'
#
# [test.metadata.expectation]
# ident = 'blo'

# TODO: it would be lovely if this could statically typecheck, but it requires
#       row subtyping, which is non-trivial.
(fun x =>
  (x |> match { 'bla => 1, 'bli => 2 }) +
  (x |> match { 'bla => 6, 'blo => 20 })) 'bla : Number