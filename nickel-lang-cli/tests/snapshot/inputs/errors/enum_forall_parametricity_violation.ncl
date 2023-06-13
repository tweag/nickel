# capture = 'stderr'
# command = []
let f : forall r. [| 'x; r |] -> [| 'y ; r |] =
  match { 'x => 'y, _ => 'z }
in f 'x
