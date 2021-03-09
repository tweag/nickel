let Assert = fun l x => x || %blame% l in

(let AlwaysTrue = fun l t =>
  let boolT | Bool = t in
  if boolT then boolT else %blame% l in
let AlwaysFalse = fun l t =>
  let boolT | Bool = t in
  if boolT then %blame% l else boolT
in let not | #AlwaysTrue -> #AlwaysFalse = fun b => if b then false else true in
not true == false) &&

// id_polymorphic_contract
(let id | forall a. a -> a = fun x => x in id true) &&

// higher_order_contract
(let to_bool | forall a. (a -> Bool) -> a -> Bool =
  fun f => fun x => f x in
  to_bool (fun x => true) 4) &&

// apply_twice
(let twice | forall a. (a -> a) -> a -> a =
  fun f => fun x => f (f x) in
twice (fun x => x + 1) 3 == 5) &&

// strings
("hello" | Str) == "hello" &&
("hello" ++ " world" | Str) == "hello world" &&

// enums_simple
(`foo | <foo, bar>) == `foo &&
(`bar | forall r. <foo, bar | r>) == `bar &&

// enums_complex
(let f : forall r. <foo, bar | r> -> Num =
  fun x => switch { foo => 1, bar => 2, _ => 3, } x in
f `bar == 2) &&

(let f : forall r. <foo, bar | r> -> Num =
  fun x => switch { foo => 1, bar => 2, _ => 3, } x in
f `boo == 3) &&

// records_simple
(({} | {}) == {} | #Assert) &&
(let x | {a: Num, s: Str} = {a = 1; s = "a"} in
  %deepSeq% x x == {a = 1; s = "a"}
  | #Assert) &&

(let x | {a: Num, s: {foo: Bool}} = {a = 1; s = { foo = true}} in
  %deepSeq% x x == {a = 1; s = { foo = true}}
  | #Assert) &&

// polymorphism
(let id | forall a. { | a} -> { | a} = fun x => x in
  let extend | forall a. { | a} -> {foo: Num | a} = fun x =>
    x & {foo = 1} in
  let remove | forall a. {foo: Num | a} -> { | a} = fun x =>
    x -$ "foo" in

  (id {} == {} | #Assert) &&
  (id {a = 1; b = false} == {a = 1; b = false} | #Assert) &&
  (extend {} == {foo = 1} | #Assert) &&
  (extend {bar = false} == {foo = 1; bar = false} | #Assert) &&
  (remove {foo = 1} == {} | #Assert) &&
  (remove {foo = 1; bar = 1} == {bar = 1} | #Assert) &&
  (remove (extend {}) == {} | #Assert) &&
  (extend (remove {foo = 2}) == {foo =1} | #Assert) &&
  (let f | forall a b. {f: a -> a, arg: a | b} -> a =
      fun rec => rec.f (rec.arg) in
    f { f = fun x => x ++ " suffix"; arg = "foo" }
    == "foo suffix"
    | #Assert)
) &&

// records_dynamic_tail
(({a = 1; b = "b"} | {a: Num, b: Str | Dyn}) == {a = 1; b = "b"}
  | #Assert) &&
(({a = 1; b = "b"; c = false} | {a: Num, b: Str | Dyn})
  == {a = 1; b = "b"; c = false}
  | #Assert) &&
((fun r => r.b | {a: Num | Dyn} -> Dyn) {a = 1; b = 2} == 2
  | #Assert) &&


// lists
(([1, "2", false] | List) == [1, "2", false] | #Assert) &&
(([1, 2, 3] | List Num) == [1, 2, 3] | #Assert) &&
((["1", "2", "false"] | List Str) == ["1", "2", "false"] | #Assert) &&

// full_annotations
// Check that the contract introduced by the type annotation doesn't interact
// with the `default` attribute
(({foo : {bar: Bool} | default = {bar = false}} & {foo.bar = true}).foo.bar
  | #Assert) &&


true
