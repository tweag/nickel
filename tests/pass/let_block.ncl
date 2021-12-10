let letfoo = 3,
    letbar : Num = 2,
    lettoto : Bool = false,
    lettutu = true,
    lettest = "i'm a test",
    f = fun x => x,
    f2 : Num -> Bool = fun x => x == 2,
in

let letres = f2 2,
    letisbar = fun y => y == letbar,
in

let letx = 5 in

{
    res = letres,
    isbary = letisbar 2,
    isbarn = letisbar 3,
    toto = lettoto,
    tutu = lettutu,
    test = lettest,
    f3 = f 3,
    x = letx
}
