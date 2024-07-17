# test.type = 'pass'
let test : {_ : {foo : Number}} = {a = {foo = 5}} in
let test_func : {_ : {_ : Number}} -> {_ : {_ : Number}} = fun a => a in 
let result : {_ : {_ : Number}} = test_func test in 
true

