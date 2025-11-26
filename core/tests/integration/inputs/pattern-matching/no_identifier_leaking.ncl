# test.type = 'pass'
let foo = true in
{ foo = false, bar = 'Blah }
|> match {
  { foo, bar = 'Bar } => foo,
  { bar = 'Blah, .. } => foo,
}
