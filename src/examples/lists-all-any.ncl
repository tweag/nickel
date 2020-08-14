let Y = fun f => (fun x => f (x x)) (fun x => f (x x)) in
let foldr_ =
    fun self f acc l =>
        if isZero (length l) then acc
        else
            let h = head l in
            let t = tail l in
            let next_acc = self f acc t in
            f next_acc h
in
let foldr = Y foldr_ in
let and = Promise(Bool -> Bool -> Bool,
    fun x y =>
        if x then
            if y then true else false
        else false)
in
let or = Promise(Bool -> Bool -> Bool,
    fun x y =>
        if x then
            true
        else
            if y then true else false)
in
let all = fun pred l => foldr and true (map pred l) in
let any = fun pred l => foldr or false (map pred l) in
let isZ = fun x => isZero x in
or (any isZ [1, 1, 1, 1]) (all isZ [0, 0, 0, 0])
