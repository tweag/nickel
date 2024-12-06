# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'

# Test that even if freezing makes the initial `String` contract to not
# propagate, it doesn't prevent other contracts coming from unfrozen records to
# propagate.
%force% ((%record/freeze% { x | String = "a" })
& { x | priority 1 | Number = 2 }
& { x | priority 2 = false })

