# test.type = 'pass'
let test : _ =
  let test_func : forall b. {a : {_ : Number}; b} -> {a : {_ : Number}; b} = fun a => a in
  test_func {a = {foo = 5}, b = 5} 
in
true
