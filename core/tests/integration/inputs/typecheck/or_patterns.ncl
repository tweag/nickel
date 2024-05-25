# test.type = 'pass'
let typecheck = [
  match {
    ('Foo x)
    or ('Bar x)
    or ('Baz x) => null,
  } : forall a. [| 'Foo a, 'Bar a, 'Baz a |] -> Dyn,

  # open enum rows when using wildcard in or-patterns

  match {
    ('Some {foo = 'Bar 5, nested = 'One ('Two null)})
    or ('Some {foo = 'Baz "str", nested = 'One ('Three null)})
    or ('Some {foo = _, nested = 'One _}) => true,
    _ => false,
  } : forall r1 r2 r3.
      [| 'Some {
        foo: [| 'Bar Number, 'Baz String; r1 |],
        nested: [| 'One [| 'Two Dyn, 'Three Dyn; r2 |] |] };
        r3
      |] -> Bool,

  match {
    {foo, bar = x, baz = [y, ..rest]}
    or {foo, bar = x @ rest, baz = [y]}
    or {foo = y @ foo, bar = x, baz = [..rest]} =>
      null,
  } : forall a. {foo: a, bar: Array a, baz: Array a} -> Dyn,
] in

true
