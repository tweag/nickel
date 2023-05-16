# test = 'error'
#
# [metadata]
# error = 'EvalError::IllegalPolymorphicTailAccess'
let f | forall a. { ; a } -> { ; a } = fun x => %seq% x."a" x 
in f { a = 1 }
