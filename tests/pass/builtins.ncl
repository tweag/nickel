let Assert = fun l x => x || %blame% l in

// is_record
(builtins.isRecord {} | #Assert) &&

/// This currently do not check that subexpressions are actually forced,
/// just that the evaluation succeeds
(%seq% 1 true | #Assert) &&
(let x = (1 + 1) in %seq% x x == 2 | #Assert) &&
(let r = {a=(1 + 1),} in
  %deepSeq% r (r.a) == 2 | #Assert) &&
(let r = {a=(1 + 1),b=("a" ++ "b"),} in
  %deepSeq% r (r.b) == "ab" | #Assert) &&
(let r = {a = {b = 1 + 1}} in
  %deepSeq% r (r.a.b) == 2 | #Assert) &&

(let inj = fun x => {b=(x + 2)} in
  let cat = fun x => fun y => x ++ y in
  let r = {a=(inj 1),b=(cat "a" "b")} in
  %deepSeq% r (r.a.b) == 3 | #Assert) &&

([1,2,3]
 |> lists.map (fun x => x + 1)
 |> lists.filter (fun x => x > 2)
 |> builtins.serialize `Json
 |> builtins.deserialize `Json
 == [3,4]
 | #Assert) &&

true
