# test = 'error'
#
# [metadata]
# error = 'EvalError::BlameError'
%force% (({ foo } | { _ | Number }) & { foo = "string" }) true