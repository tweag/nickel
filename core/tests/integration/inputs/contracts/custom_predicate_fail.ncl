# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let AlwaysFail = std.contract.from_predicate (fun _ => false) in
3 | AlwaysFail
