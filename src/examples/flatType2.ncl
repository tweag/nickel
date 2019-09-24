let alwaysTrue = fun l => fun t => let boolT = Assume(Bool, t) in 
    if boolT then boolT else blame l in
let alwaysFalse = fun l => fun t => let boolT = Assume(Bool, t) in 
    if boolT then  blame l else boolT in
let not = fun b => if b then false else true in
Assume(#alwaysTrue -> #alwaysFalse, not ) true
