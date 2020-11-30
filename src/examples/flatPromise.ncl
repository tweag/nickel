let alwaysTrue = fun l t => let boolT = Assume(Bool, t) in
    if boolT then boolT else blame l in
let id : #alwaysTrue -> Bool -> #alwaysTrue = fun b x => if x then b else b in
(id : #alwaysTrue -> Bool -> #alwaysTrue) false true
