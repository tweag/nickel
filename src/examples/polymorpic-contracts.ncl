nickel <<< '
let unw = Assume(forall a. Dyn -> a, fun x => x) in
let getWrpd = fun rec => unw (rec.$"%_wrapped") in
let f = Assume(forall a. { {| a: Num | a} } -> Dyn, fun rec => rec.$"_%wrapped")
  in
unw (f { a = 1; b = 2; c = 3})
'

nickel <<< '
let f = Assume(
    forall a. ((forall b. ({ {| a: Num, b: Num, |b} }) -> ({{| a: Num, |b} })) ->
        { {| a: Num | a}} -> { {| |a} }),
    fun f rec => (f rec) -$ "a" -$ "b")
in
f (fun x => x) {a = 1; b = bool; c = 3}
'

nickel <<< '
let f = Assume(
    forall a .(forall b. { {| f: a -> a, arg: a | b} } -> Dyn),
    fun rec => rec.f (rec.arg)
) in
f { f = fun x => x ++ " suffix"; arg = "blouh" }
'
