# test.type = 'pass'
# eval = 'typecheck'
let f : forall a. a -> a = fun x => x in
{ 
  include f,
  x = f 1,
  y = f "hello",
  z = f ((+) 1),
} : _
