# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
%force% (({ foo } | { _ | Number }) & { foo = "string" }) true