# test.type = 'pass'
let Assert = std.test.Assert in

[
  {val | default | Number = 10}.val == 10,

  # merge_default
  ({a = 2} & {a | default = 0, b | default = true}) == {a = 2, b = true},
  {a | default = {x = 1}} & {a | default = {y = "y"}} == {a = {x = 1, y = "y"}},

  # merge_contract
  {a = 2, b | Bool} & {a | Number, b | default = true}
    == {a = 2, b = true},

  # merge_default_contract
  {a = 2} & {a | default | Number = 0, b | default = true}
    == {a = 2, b = true},

  {a=2} & {a | Number} & {a | default = 3} == {a = 2},
  {a=2} & {b | Number} & {b | default = 3} == {a = 2, b = 3},
  ({a | default = 1} & {b | Number} & {a | default = 1}).a
    == 1,

  # composed
  let Even = fun l x => if x % 2 == 0 then x else %blame% l in
  let DivBy3 = fun l x => if x % 3 ==  0 then x else %blame% l in
  let composed = {a | Even} & {a | DivBy3} in
  (composed & {a = 6} == {a = 6} | Assert) &&
  (composed & {a = 12} == {a = 12} | Assert),

  # Check that the environments of contracts are correctly saved and restored when merging. See
  # issue [#117](https://github.com/tweag/nickel/issues/117)
  (let ctr_num = let x = Number in {a | x} in
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

  # optionals
  let Contract = {foo | Number, opt | String | optional} in
  let value | Contract = {foo = 1} in
  (
    (value == {foo = 1} | Assert) &&
    (std.serialize 'Json value
      == std.serialize 'Json {foo = 1}
      | Assert) &&
    (std.record.has_field "foo" value | Assert) &&
    (!(std.record.has_field "opt" value) | Assert) &&
    (std.record.values value == [1] | Assert) &&
    (std.record.fields value == ["foo"] | Assert)
  ),

  # Although unintuitive, we have to:
  # 1. Make Contract open (the `..` at the end)
  # 2. Repeat foo in `{foo = 1, baz = false}` with the same value
  # All of this because of #710 (https://github.com/tweag/nickel/issues/710). We
  # may get rid of both once the merge semantics is clarified.
  let Contract = {foo | Number, opt | String | optional, ..} in
  let with_ctr | Contract = {foo = 1} in
  let value | Contract = with_ctr & {foo = 1, baz = false} in
  (
    (value == {foo = 1, baz = false} | Assert) &&
    (std.serialize 'Json value
      == std.serialize 'Json {foo = 1, baz = false}
      | Assert) &&
    (std.record.has_field "foo" value | Assert) &&
    (std.record.has_field "baz" value | Assert) &&
    (!(std.record.has_field "opt" value) | Assert) &&
    (std.record.values value == [false, 1] | Assert) &&
    (std.record.fields value == ["baz", "foo"] | Assert)
  ),

  let Contract = {foo | Number, opt | String | optional} in
  let value | Contract = {foo = 1 + 0, opt = "a" ++ "b"} in
  (
    (value == {foo = 1, opt = "ab"} | Assert) &&
    (std.serialize 'Json value
      == std.serialize 'Json {foo = 1, opt = "ab"}
      | Assert) &&
    (std.record.has_field "foo" value | Assert) &&
    (std.record.has_field "opt" value | Assert) &&
    (std.record.values value == [1, "ab"] | Assert) &&
    (std.record.fields value == ["foo", "opt"] | Assert)
  ),

  let Contract = {foo | Number, opt | String | optional} in
  let with_ctr | Contract = {foo = 0.5 + 0.5} in
  # Same as above: we have to repeat `foo` with the same value because of #710
  let value = with_ctr & {foo = 1, opt = "a" ++ "b"} in
  (
    (value == {foo = 1, opt = "ab"} | Assert) &&
    (std.serialize 'Json value
      == std.serialize 'Json {foo = 1, opt = "ab"}
      | Assert) &&
    (std.record.has_field "foo" value | Assert) &&
    (std.record.has_field "opt" value | Assert) &&
    (std.record.values value == [1, "ab"] | Assert) &&
    (std.record.fields value == ["foo", "opt"] | Assert)
  ),

  let Contract = {foo | Number, opt | String | optional} in
  let with_ctr | Contract = {foo = 0.5 + 0.5} in
  # Same as above: we have to repeat `foo` with the same value because of #710
  # repeating `opt` without an `optional` attribute makes it required, but as
  # long as we don't extract it (nor map onto the record), everything should be
  # fine
  let value = with_ctr & {foo = 1, opt} in
  (
    # std.record.has_field/std.record.fields have a dictionary contract `{_ : T}`
    # attached, which currently uses `std.record.map` under the hood, which will
    # throw an error for `opt` missing a defnition. They
    # probably shouldn't, but for the time being, we bypass them using the
    # lowlevel primop. Uncomment the 3 following line once #XXX is closed
    # (https://github.com/tweag/nickel/issues/892)
    # (std.record.has_field "foo" value | Assert) &&
    # (std.record.has_field "opt" value | Assert) &&
    # (std.record.fields value == ["foo", "opt"] | Assert)
    (%record/has_field% "foo" value | Assert) &&
    (%record/has_field% "opt" value | Assert) &&
    (%record/fields% value == ["foo", "opt"] | Assert)
  ),
]
|> std.test.assert_all
