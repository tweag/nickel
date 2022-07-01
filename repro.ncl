let A = fun param label value => value in
let B = fun label value => value in

let MyArray = fun param => null in

let id | forall a. MyArray (let foo = (let bar = a in 5) in null) -> MyArray a = fun x => x in
null

# [1] | Array (A Dyn)
#[ [1] | A (Array B),
#  [1] | A (B -> A),
#  [1] | A ({_ : B}),
#]

## Array unevaluated bug

# doesn't work
let MyRec = fun label value => value in ([{x = "hello" ++ "foo"}] | Array MyRec)

# works
([{x = 1 + 2}] | Array {x : Num}) |> builtin.serialize `Json

# works
let Strict = contract.from_predicate (fun value =>
  builtin.is_record value && builtin.is_str value.x) in
([{x = "foo" ++ "bar"}] | Array Strict) |> builtin.serialize `Json

let with_ctr = ([{x = "hello" ++ "foo"}] | Array {..})

