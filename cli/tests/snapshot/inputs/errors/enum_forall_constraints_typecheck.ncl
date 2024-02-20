# capture = 'stderr'
# command = ['eval']
let f | forall r. [| ; r |] -> [| 'Foo Number; r |] = fun tag =>
  if true then
    tag
  else
    'Foo 1
in
(f ('Foo "hello") : _)
