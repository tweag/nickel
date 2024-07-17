# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let AlwaysFail = std.contract.custom (fun label _ => std.contract.blame label) in
3 | AlwaysFail
