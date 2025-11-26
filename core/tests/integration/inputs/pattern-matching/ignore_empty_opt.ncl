# test.type = 'pass'
{ foo = 1, bar | optional }
|> match {
  { foo } => true
}
  | std.test.Assert
