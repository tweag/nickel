# test.type = 'pass'
let Assert = std.test.Assert in

[
  let block1 = {
    foo | default = 1,
    bar = 1,
    baz | force = 1,
    x | priority 10 = 1,
    y | priority -5 = 1,
    z | priority 0 = 1,
    d | default = 1,
  } in

  let block2 = {
    foo | priority -10 = 2,
    bar | priority 10 = 2,
    baz = 2,
    x | priority 11 = 2,
    y  = 2,
    z | priority 10 = 2,

  } in

  let block3 = {
    foo | priority -10.1 = 3,
    bar | default = 3,
    baz | priority 1000 = 3,
    x | priority 12 = 3,
    y | priority -1 = 3,
    z | priority 50 = 3,
  } in

  block1 & block2 & block3
  == {
    foo = 2,
    bar = 2,
    baz = 1,
    x = 3,
    y = 2,
    z = 3,
    d = 1,
  } | Assert,

  # All the following commented out tests are related to recursive priorities,
  # which are on hold since RFC005 (and were disabled in 1.2.0). We need to
  # rethink their semantics before proceeding further.

  #{foo | rec default = 1} & {foo = 2} == {foo = 2} | Assert,
  #{foo | rec force = 1} & {foo = 2} == {foo = 1} | Assert,
  #{val | rec default = {foo = 1}} & {val.foo = 2} == {val.foo = 2} | Assert,
  #{val | rec force = {foo = 1}} & {val.foo = 2} == {val.foo = 1} | Assert,

  # Pushed priorities should only modifies fields without explicitly set priorities
  #{val | rec force = {foo | priority -1 = 1}} & {val.foo = 2} == {val.foo = 2} | Assert,
  #{val | rec force = {foo | default = 1}} & {val.foo = 2} == {val.foo = 2} | Assert,
  #{val | rec default = {foo | priority 1 = 1}} & {val.foo = 2} == {val.foo = 1} | Assert,
  #{val | rec default = {foo | force = 1}} & {val.foo = 2} == {val.foo = 1} | Assert,

  #let x = {
  #  foo | force = "old",
  #  bar = {
  #      baz = "old",
  #      baz' = "old"
  #    } |> std.record.map (fun _name value => value)
  #} in
  #{val | rec default = x} & {val = {foo = "new", bar.baz = "new"}}
  #  == { val = {foo = "old", bar = {baz = "new", baz' = "old"}}}
  #  | Assert,

  # Interaction of recursive overriding and recursive priorities
  #
  # Broken by the implementation of RFC005. First tests seem to show that `x` is
  # correctly evaluated to `{foo | force = context + 2 + bar, bar | default =
  # 1}`. This is not an overriding bug, as writing the term above directly
  # (without `rec force` but with `force` directly) works.
  # Maybe some field update issue?
  #
  # let context = 1 in
  # let x = {
  #   foo = context + 2 + bar,
  #   bar | default = 1,
  # } in
  # {val | rec force = x } & {val = {foo = 0, bar = 10}}
  #   == {val = {bar = 10, foo = 1 + 2 + 10}} | Assert,
] |> std.test.assert_all
