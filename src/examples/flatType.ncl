let alwaysTrue = fun l t => let boolT = Assume(Bool, t) in
    if boolT then boolT else blame l in
Assume(#alwaysTrue, true)
