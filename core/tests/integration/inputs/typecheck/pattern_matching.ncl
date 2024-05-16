# test.type = 'pass'

# static typing of pattern matching
let typecheck = [
  match {
    'Foo => null,
    'Bar => null,
    'Baz => null,
  } : [| 'Foo, 'Bar, 'Baz |] -> Dyn,

  match {
    'Foo => null,
    'Bar => null,
    'Baz => null,
    _ => null,
  } : [| 'Foo, 'Bar, 'Baz |] -> Dyn,

  match {
    'Foo => null,
    'Bar => null,
    'Baz => null,
    _ => null,
  } : forall r. [| 'Foo, 'Bar, 'Baz; r |] -> Dyn,

  match {
    'Foo x => x,
    'Bar y => y + 1,
  } : [| 'Foo Number, 'Bar Number |] -> Number,

  match {
    {x, y = 'Foo y, ..rest} => y + rest.bar,
    {x, y = 'Bar y, ..rest} => y + rest.bar,
  } : forall a b rows.
        {x: a, y : [| 'Foo Number, 'Bar Number |], bar: Number, unused: b; rows}
        -> Number,
 
  match {
    {x, y, ..rest} => null,
    {x, z, ..rest} => null,
  } : forall a b c. {x: a, y: b, z: c} -> Dyn,

  # widening through polymorphism

  (let return_stuff
    : forall ext. Number -> [| 'Pos, 'Neg; ext |]
    = fun x => if x > 0 then 'Pos else 'Neg in
    (return_stuff 1 |> match {
      'Pos => "pos",
      'Neg => "neg",
      'Complex => "complex",
    })
    ++ (return_stuff (-1) |> match {
      'Pos => "pos",
      'Neg => "neg",
      'Real => "real",
    })
  ) : _,

  # open enum rows when using wildcard

  match {
    'Some {foo = 'Bar 5, nested = 'One ('Two null)} => true,
    'Some {foo = 'Baz "str", nested = 'One ('Three null)} => true,
    'Some {foo = _, nested = 'One _} => false,
    _ => false,
  } : forall r1 r2 r3.
      [| 'Some {
        foo: [| 'Bar Number, 'Baz String; r1 |],
        nested: [| 'One [| 'Two Dyn, 'Three Dyn; r2 |] |] };
        r3
      |] -> Bool,

  # array patterns

  match {
    [] => true,
  } : forall a. Array a -> Bool,

  match {
    [] => true,
    _ => false,
  } : forall a. Array a -> Bool,

  match {
    [x] => x,
    [x, y] => y,
    [..rest] => std.array.first rest,
  } : forall a. Array a -> a,

  match {
    [x] => x,
    [x, y, ..] => y + 1,
    _ => 0,
  } : Array Number -> Number,
  
  match {
    ['Foo x] => x,
    ['Bar y, ..] => y,
  } : forall a. Array [| 'Foo a, 'Bar a |] -> a,
  
  match {
    ['Foo x] => "foo",
    ['Bar y] => "bar",
    [_] => "other",
  } : forall a r. Array [| 'Foo a, 'Bar a; r |] -> String,
] in

true
