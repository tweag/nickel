typealias Type1 = { foo: Num, bar: Str } in
typealias Type2 = { rec: Type1, foobool: Bool } in
typealias Type3 = { recrec: Type2, rec: Type1, foo: Num } in

typealias FctType = forall a b. (a -> b) -> List a -> List b in

let maptest : forall a b. (a -> b) -> List a -> List b = fun f l => %map% l f in
//let map1 : FctType = fun f l => %map% l f in
//let map2 : FctType = fun f l => %map% l f in

let f = fun x => x in

{
    mylist0 = maptest (fun z => z*4) [4, 6],
//    mylist1 = map1 (fun x => x*2) [3, 5],
//    mylist2 = map2 (fun y => y*5) [5, 10],

//    outofscope_type = (typealias foo = Num in (f (0 : foo)) : foo) + (f 5),

    data : Type3 = {
        recrec = {
            rec = {
                foo = 3,
                bar = "recrec",
            },
            foobool = false,
        },
        rec = {
            foo = 5,
            bar = "rec",
        },
        foo = 42,
    },
}
