let alwaysTrue = fun l => fun t => let boolT = Assume(Bool, t) in 
    if boolT then boolT else blame l in
let id = Promise(#alwaysTrue -> Bool -> #alwaysTrue, fun b => fun x => if x then b else b) in
Promise(#alwaysTrue -> Bool -> #alwaysTrue, id )  false true
