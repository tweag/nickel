# test.type = 'pass'
let test : _ =
    let test_func : {a : {_ : Number}} -> {a : {_ : Number}} = fun a => a in
    test_func {a = {foo = 5}}
in 
true
