let Assert = fun l x => x || %blame% l in

// basic
(0 == 0 + 0 + 0 | #Assert) &&
(true == (if true then true else false) | #Assert) &&
("a" ++ "b" ++ "c" == "#{"a" ++ "b"}" ++ "c" | #Assert) &&
(`Less == `Less | #Assert) &&
(`small == `small | #Assert) &&

(1 + 1 != 0 | #Assert) &&
(true != (if true then false else true) | #Assert) &&
("a" != "a" ++ " " | #Assert) &&
(1 != true | #Assert) &&
("1" != 1 | #Assert) &&
("true" != true | #Assert) &&
(`Less != `small | #Assert) &&
(`Less != 0 | #Assert) &&
(`Greater != false | #Assert) &&

// lists
([] == [] | #Assert) &&
([(1 + 0), (1 + 1), (1 + 1 + 1)] == [1, 2, 3] | #Assert) &&
([(1 + 0), ("a" ++ "b"), (if true then true else false)]
  == [1, "ab", true]
  | #Assert) &&
([[[]]] == [[[]]] | #Assert) &&
([[1], [[2]]] == [[2 + (-1)], [[1 + 1]]] | #Assert) &&
([[true, false]] == lists.flatten [[[true, false]]] | #Assert) &&

([] != [1] | #Assert) &&
([] != 1 | #Assert) &&
([] != {} | #Assert) &&
([1, "a", true] != [1, "a", false] | #Assert) &&
([[true]] != [[[true]]] | #Assert) &&

// records
({} == {} | #Assert) &&
({}$["a" = 1]$["b" = true]
  == {a = 1; b = true}
  | #Assert) &&
({a = 1 + 0; b = 1 + 1; c = 1 + 1 + 1}
  == { a = 1; b = 2; c = 3 }
  | #Assert) &&
({
    foo = 1 + 0;
    bar = "a" ++ "b";
    baz = if true then true else false
  } == {foo = 1; bar = "ab"; baz = true}
  | #Assert) &&

({}$["a" = { a = { a = {} } }]
  == { a = { a = { a = {} } } }
  | #Assert) &&
({
    foo = {bar = 2 + (-1)};
    baz = {foo = {bar = 1 + 1}}
  }
  == {foo = {bar = 1}; baz = {foo = {bar = 2}}}
  | #Assert) &&
({} != {a = true} | #Assert) &&
({a = 1} != {a = 2} | #Assert) &&
({ a = "a"; b = true } != { a = true; b = "a"} | #Assert) &&
({ a = { a = true } } != {a = { a = { a = true } } } | #Assert) &&

// Now that the equality operator directly uses the stack to store its continuation (see
// https://github.com/tweag/nickel/pull/247), check that it correctly cleans the stack when
// evaluating a subequality to `false`.
// Generate an non-empty evaluation context to evaluate equalities over a non-empty stack
(let eq_with_ctxt = fun x y =>
    let not = fun b =>
      if b then true else false in
    not (not (not (not ((x) == (y))))) in
  (eq_with_ctxt
      {a = 1 + 0; b = 1 + 1 + a; c = 0; d = 0}
      { a = 1; b = 3; c = 0; d = 0}
    | #Assert) &&
  (eq_with_ctxt
      [[1,2,3,4], [1,0,3,4], [1,2,3,4], [1,2,3,4]]
      [[1,2,3,4], [1,2,3,4], [1,2,3,4], [1,2,3,4]]
    == false 
    | #Assert)) &&

true
