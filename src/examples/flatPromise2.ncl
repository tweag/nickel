let alwaysTrue = fun l t => let boolT = Assume(Bool, t) in
    if boolT then boolT else blame l in
let id : #alwaysTrue -> Bool -> #alwaysTrue = fun b x => if x then Assume(#alwaysTrue, true) else b in
(id Assume(#alwaysTrue, false) : Bool -> #alwaysTrue) true
