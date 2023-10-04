let Sub1 = { .. } in
let Contract1 = { x | Number, x2 | Sub1 = let blah = x in {} } in
{
  foo = { x = 1 } | Contract1,
  bar = foo | Contract1,
}
