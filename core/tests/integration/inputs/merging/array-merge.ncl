# test.type = 'pass'


[
  # array pointwise merging
  let x = std.array.generate (fun x => 2*x + 1) 10 in x & x == x,
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
|> std.test.assert_all
