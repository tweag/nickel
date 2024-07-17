# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let AlwaysFail = std.contract.custom (fun _ _ => 'Error {}) in
3 | AlwaysFail
