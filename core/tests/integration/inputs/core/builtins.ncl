# test.type = 'pass'


[
  # is_record
  std.is_record {},

  # This currently does not check that subexpressions are actually forced,
  # just that the evaluation succeeds
  %seq% 1 true,
  let x = (1 + 1) in %seq% x x == 2,
  let r = {a=(1 + 1),} in
    %deep_seq% r (r.a) == 2,
  let r = {a=(1 + 1),b=("a" ++ "b"),} in
    %deep_seq% r (r.b) == "ab",
  let r = {a = {b = 1 + 1}} in
    %deep_seq% r (r.a.b) == 2,

  let inj = fun x => {b=(x + 2)} in
    let cat = fun x => fun y => x ++ y in
    let r = {a=(inj 1),b=(cat "a" "b")} in
    %deep_seq% r (r.a.b) == 3,

  [1,2,3]
   |> std.array.map (fun x => x + 1)
   |> std.array.filter (fun x => x > 2)
   |> std.serialize 'Json
   |> std.deserialize 'Json
   == [3,4],
]
|> std.test.assert_all
