# capture = 'stderr'
# command = []
let val | Array (Array {foo: Dyn -> Number }) = [
  [],
  [{ foo = fun x => "string" }]
] in
std.array.at 1 val
|> std.array.at 0
|> (fun r => r.foo null)
