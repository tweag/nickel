let Y = fun f => (fun x => f (x x)) (fun x => f (x x)) in
let foldl_ =
    fun self => fun f => fun acc => fun l =>
        if isZero (length l) then acc
        else
            let h = head l in
            let t = tail l in
            let next_acc = f acc h in
            seq next_acc (self f next_acc t)
in
let foldl = Y foldl_ in
foldl (fun x => fun y => x + y) 0 ([1, 2, 3, 4, 5])
