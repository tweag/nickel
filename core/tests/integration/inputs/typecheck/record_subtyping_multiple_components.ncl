# test.type = 'pass'
let test : _ =
  let test_func : {a : {_ : Number}, b : {_ : String}} -> {a : {_ : Number}, b : {_ : String}} = fun a => a in 
  test_func {a = {foo = 5}, b = {a = "test"}}
in
true
