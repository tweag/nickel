# has_field operator (`?`)
[
  ({ a = 1; } ? a == true)
  ({ a = 1; } ? "a" == true)
  ({ a = 1; } ? b == false)
  ({ a = 1; } ? "b" == false)
  ({ a.foo = 1; } ? a.foo == true)
  ({ a.foo = 1; } ? a."foo" == true)
  ({ a.foo = 1; } ? "a.foo" == false)
  ({ a.foo = 1; } ? "a".foo == true)
  ({ a.foo = 1; } ? a == true)
  ({ a.foo = 1; } ? a.bar == false)

  # simple field access
  ({ a = 1; }.a == 1)
  ({ a.b = 2; }.a == { b = 2; })
  ({ a.b = 2; }.a.b == 2)

  # inherits fields
  (let a = 1; in { inherit a; } == { a = 1; })
  (let a = { b = 2; }; in { inherit a; }.a == { b = 2; })
  (let b = 2; in { a = { inherit b; }; } == { a.b = 2; })

  # inherits fields from record
  (let r = { a = 1; b = 2; c = 3; }; in { inherit (r) a c; } == { a = 1; c = 3; })
  # TODO the folowing are not working, probably because of an issue in let bindings
  # or may be specificaly an issue when we use inherit inside a let binding. this maybe due to
  # the order we desugar destructuring and inherits. Let bindings are transformed in Nickel destructuring
  # this to have recursive lets which don't exist in Nickel.
  (
    let
      r1 = { a = 1; };
      r2 = { inherit (r1) a; b = 2; };
    in
    { inherit (r2) a b; } == { a = 1; b = 2; }
  )
  #(let
  #  r1 = {a = 1;};
  #  r2 = {inherit (r1) a; b = 2;};
  #  r3 = {inherit r2;};
  #in
  #{ inherit (r3.r2) a b;} == {a = 1; b = 2;})

  # test of alternative operator `or`
  #({a = 1;}.a or 2 == 1)
  #({a = 1;}.b or 2 == 2)
]
