# test.type = 'pass'
let test : _ = 
  let test_func : {_ : {_ : Number}} -> {_ : {_ : Number}} = fun a => a in 
  test_func {a = {foo = 5}}
in
true

