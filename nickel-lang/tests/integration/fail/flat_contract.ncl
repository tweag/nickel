# test.type = 'error'
#
# [test.metadata]
# error = 'EvalError::BlameError'
let 
  AlwaysTrue = fun l t => 
    let boolT | Bool = t 
    in if boolT then boolT else %blame% l 
in (false | AlwaysTrue)
