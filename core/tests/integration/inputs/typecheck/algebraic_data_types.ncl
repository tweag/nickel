# test.type = 'pass'
let typecheck = [
  ('a |> match
      {
        'a => 'Foo 2,
        'b => 'Bar "hello",
        'c => 'Baz true,
      }
  ) : [| 'Foo Number, 'Bar String, 'Baz Bool, 'OtherTail |],
  (fun arg => 'Foo arg) : forall a b. a -> [| 'Foo a; b |],
  'Foo ('Bar 'Baz) : [| 'Foo [| 'Bar [| 'Baz |] |] |],
] in

true
