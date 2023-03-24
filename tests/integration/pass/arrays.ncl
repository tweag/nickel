let {check, ..} = import "lib/assert.ncl" in

[
  # accesses
  array.at 1 [1,2,3] == 2,
  array.at 1 (array.map (fun x => x + 1) [1,2,3]) == 3,

  # length
  array.length [] == 0,
  array.length [1,2,3] == 3,
  array.length ([] @ [1,2] @ [3,4] @ []) == 4,

  # sort
  let cmp = fun x y =>
    if x < y then `Lesser
    else if x == y then `Equal
    else `Greater
  in array.sort cmp [3, 42, -1, -5] == [-5, -1, 3, 42],

  # Test case added after https://github.com/tweag/nickel/issues/154
  let x = 1 in let l = [x] @ [2] in (%elem_at% l 0) == 1,

  let Y = fun f => (fun x => f (x x)) (fun x => f (x x)) in
    let foldr_ =
      fun self => fun f acc array =>
        let length = %length% array in
        if length == 0 then
          acc
        else
          let head = %elem_at% array 0 in
          let tail = %array_slice% 1 length array in
          let next_acc = self f acc tail in
          f next_acc head in
    let foldr = Y foldr_ in
    let and : Bool -> Bool -> Bool =
        fun x => fun y =>
            if x then
                if y then true else false
            else false
    in
    let all = fun pred array => foldr and true (%map% array pred) in
    let isZ = fun x => x == 0 in
    all isZ [0, 0, 0, 1] == false,

  # array pointwise merging
  let x = array.generate (fun x => 2*x + 1) 10 in x & x == x,
  [1 / 1, 2*1, 1/3*9] & [0 + 1, 1 + 1, 1 + 2] == [1, 2, 3],
  # test that double application of a contract with a non-empty array as a
  # default value doesn't fail (regression test for
  # https://github.com/tweag/nickel/issues/1187)
  let CustomNumber =
  # the _ign is just there to enforce CustomNumber isn't a value
    let _ign = null in
    fun _label value => (value | Number)
  in
  let Contract = { foo | Array CustomNumber | default = [1,2,3,4]} in
  let data | Contract
           | Contract = {}
  in 
  data == data,
  # ensure lazy contracts are applied on both side
  let DivideByTwo = fun label value => value / 2 in
  let AddOne = fun label value => value + 1 in
  ([1 + 1, 2 + 2, 3 + 3] | Array DivideByTwo)
  & ([0 + 0, 1 + 0, 2 + 0] | Array AddOne)
  == [1, 2, 3],
]
|> check
