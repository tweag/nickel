let Y = fun f => (fun x => f (x x)) (fun x => f (x x)) in
let foldr_ =
    fun self f acc l =>
        if length l == 0 then acc
        else
            let h = head l in
            let t = tail l in
            let next_acc = self f acc t in
            f next_acc h
in
let foldr = Y foldr_ in
let and : Bool -> Bool -> Bool = fun x y => x && y in
let or : Bool -> Bool -> Bool = fun x y => x || y in
let all = fun pred l => foldr and true (map pred l) in
let any = fun pred l => foldr or false (map pred l) in
let isZ = fun x => x == 0 in
or (any isZ [1, 1, 1, 1]) (all isZ [0, 0, 0, 0])
