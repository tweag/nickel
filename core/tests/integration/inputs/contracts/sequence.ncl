# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'

3 | std.contract.Sequence [ Number, std.contract.from_predicate (fun x => x < 5), std.contract.from_predicate (fun x => x > 5) ]
