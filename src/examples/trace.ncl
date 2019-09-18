let map = Assume((Num -> Num) -> Num, fun funky => (funky 3) + (funky false)) in
map (fun yak => yak + 1)
