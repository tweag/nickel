let val | Array (Array {foo: Dyn -> Number }) = [
  [],
  [{ foo = fun x => "string" }]
] in
array.at 1 val
|> array.at 0
|> (fun r => r.foo null)
