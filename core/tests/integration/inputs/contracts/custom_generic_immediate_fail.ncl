# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let AlwaysFail = std.contract.custom (fun _ => 'Error {}) (fun _ value => value) in
3 | AlwaysFail
