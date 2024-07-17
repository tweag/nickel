# test.type = 'pass'
let test : Array {foo : Number} = [{foo = 5}] in
let test_func : Array {_ : Number} -> Array {_ : Number} = fun a => a in 
let result = test_func test in 
true
