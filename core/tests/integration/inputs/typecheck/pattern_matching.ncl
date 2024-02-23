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
] in

true
