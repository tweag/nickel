let Assert = fun l x => x || %blame% l in

// accesses
(({foo = 3; bar = true}).bar == true | #Assert) &&
({"#{if true then "foo" else "bar"}" = false; bar = true}.foo
  == false
  | #Assert) &&

(({foo = 3; bar = true})."bar" == true | #Assert) &&
({"#{if true then "foo" else "bar"}" = false; bar = true}."#{"foo"}"
 == false 
 | #Assert) &&

(({bar = 3}$["foo" = true]).foo == true
  | #Assert) &&

// primitive_ops
(records.hasField "foo" {foo = 1; bar = 2} | #Assert) &&
(records.hasField "fop" {foo = 1; bar = 2} == false | #Assert) &&
(records.hasField "foo" ({foo = 2; bar = 3} -$ "foo")
  == false
  | #Assert) &&

(records.hasField "foo" ({bar = 3}$["foo" = 1]) | #Assert) &&

// lazyness of map
((records.map (fun x y => y + 1) {foo = 1; bar = "it's lazy"}).foo
  == 2
  | #Assert) &&

(let r = records.map
    (fun y x => if %isNum% x then x + 1 else 0)
    {foo = 1; bar = "it's lazy"} in
  (r.foo) + (r.bar) == 2
  | #Assert) &&

// merging
({a = 1} & {b=true} == {a = 1; b = true} | #Assert) &&
({a = 1; b = 2} & {b = 2; c = 3}
  == {a = 1; b = 2; c = 3}
  | #Assert) &&

({a = {b = 1}} & {a = {c = true}}
  == {a = {b = 1; c = true}}
  | #Assert) &&

// merge_complex
(let rec1 = {
    a = false;
    b = if true then (1 + 1) else (2 + 0);
    c= ((fun x => x) (fun y => y)) 2;
  } in
  let rec2 = {
    b = ((fun x => x) (fun y => y)) 2;
    c = if true then (1 + 1) else (2 + 0);
    d = true;
  } in
  let result = {
    a = false;
    b = 2;
    c = 2;
    d = true;
  } in
  rec1 & rec2 == result
  | #Assert) &&

// merge_with_env
((fun y => ((fun x => {a=y}) 1) & ({b=false})) 2
  == {a = 2; b = false}
  | #Assert) &&

// merge_with_env_nested
({b={c=10}} & ((fun x => {a=x; b={c=x}}) 10)
  == {a=10; b = {c = 10}}
  | #Assert) &&

// recursive_records
({a = 1; b = a + 1; c = b + a} == {a = 1; b = 2; c = 3} | #Assert) &&
({f = fun x y =>
    if x == 0 then y else f (x + (-1)) (y + 1)
  }.f 5 5
  == 10
  | #Assert) &&

(let with_res = fun res =>
  {
    f = fun x =>
      if x == 0 then
        res
      else g x;
    g = fun y => f (y + (-1))
  }.f 10 in
  with_res "done" == "done"
  | #Assert) &&


true
