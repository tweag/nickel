let alwaysTrue = fun l => fun t => let boolT = Assume(Bool, t) in 
    if boolT then boolT else blame l in
let id = fun b => fun x => if x then Assume(#alwaysTrue, true) else b in
Promise(Bool -> #alwaysTrue, id   Assume(#alwaysTrue, false) ) true
