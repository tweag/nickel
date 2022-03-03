let Assert = fun l x => x || %blame% l in

[
  (10 | default | Num) == 10,

  # Check the correct handling of the update of thunks containing enriched values (see issue
  # https:#github.com/tweag/nickel/issues/123)
  let x = {a = (fun x => (1 | default)) 1} in
    builtin.seq (x.a) ((x & {a=2}).a) == 2,

  # merge_default
  ({a = 2} & {a | default = 0, b | default = true}) == {a = 2, b = true},
  {a | default = {x = 1}} & {a | default = {y = "y"}} == {a = {x = 1, y = "y"}},

  # merge_contract
  {a = 2, b | Bool} & {a | Num, b | default = true}
    == {a = 2, b = true},

  # merge_default_contract
  {a = 2} & {a | default | Num = 0, b | default = true}
    == {a = 2, b = true},

  {a=2} & {a | Num} & {a | default = 3} == {a = 2},
  {a=2} & {b | Num} & {b | default = 3} == {a = 2, b = 3},
  ({a | default = 1} & {b | Num} & {a | default = 1}).a
    == 1,

  # composed
  let Even = fun l x => if x % 2 == 0 then x else %blame% l in
    let DivBy3 = fun l x => if x % 3 ==  0 then x else %blame% l in
    let composed = {a | Even} & {a | DivBy3} in
    (composed & {a = 6} == {a = 6} | Assert) &&
    (composed & {a = 12} == {a = 12} | Assert),


  # Check that the environments of contracts are correctly saved and restored when merging. See
  # issue [#117](https:#github.com/tweag/nickel/issues/117)
  (let ctr_num = let x = Num in {a | x} in
    let ctr_id = let x = fun l x => x in {a | x} in
    let val = let x = 1 in {a = x} in
    let def = let x = 2 in {a | default = x} in
    let def2 = let x = (1 + 1) in {a | default = x} in
  # contract/contract -> contract/value -> value/default
  ((ctr_num & ctr_id & val & def).a == 1 | Assert) &&
  # default/value <- value/contract
  ((def & (val & ctr_num)).a == 1 | Assert) &&
  # default/contract-> contract-default/contract-default <- contract/default
  (((def & ctr_num) & (ctr_id & def2)).a == 2 | Assert) &&
  # default/contract -> contract-default/contract -> contract-default/value
  ((def & ctr_num & ctr_id & val).a == 1 | Assert) &&
  # default/contract -> contract-default/default
  ((def & ctr_num & def2).a == 2 | Assert) &&
  # value/contract-default <- contract/contract-default
  ((val & (ctr_num & def)).a == 1 | Assert)),
]
|> array.foldl (fun x y => (x | Assert) && y) true
