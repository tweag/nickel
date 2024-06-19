# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let AlwaysFail = std.contract.from_validator (fun _ => 'Error {}) in
3 | AlwaysFail
