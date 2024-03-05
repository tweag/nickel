# test.type = 'pass'
(
  let f : forall r. [| 'Foo Number; r |] -> Bool = match {
    'Foo x => false,
    _ => true,
  } in
  
  let g : forall r. [| 'Foo Number, 'Foo |] -> Bool = match {
    'Foo x => false,
    'Foo => true,
  } in

  f 'Foo && g 'Foo
) : Bool 
