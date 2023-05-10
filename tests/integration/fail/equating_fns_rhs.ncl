test = 'error'

[metadata]
error = 'EvalError::EqError'
---
let g = fun x => x + 1 in
"a" == g