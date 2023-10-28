# test.type = 'error'
# eval = 'full'
#
# [test.metadata]
# error = 'EvalError::BlameError'

# Regression test for https://github.com/tweag/nickel/issues/1700
let False = std.contract.from_predicate (fun x => false) in
({ bli = {foo = 1} }
  | { _| {..} })
  | { _| False }
