let f = Assume((Num -> Num) -> Num, fun g => g 3) in
f (fun x => true)