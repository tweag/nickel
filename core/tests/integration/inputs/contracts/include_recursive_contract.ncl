# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let x = 1 in
({
  include x | Contract,
  Contract = Dyn,
} & {
  Contract | force = std.FailWith "",
}).x
