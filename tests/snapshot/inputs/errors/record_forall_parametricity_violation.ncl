# capture = 'stderr'
# command = []
let f : forall r. { x : String } -> { x : String; r } =
  fun x => x
in f { x = 1 }
