# capture = 'stderr'
# command = ['eval']

# Regression test for #1312
let f : forall a. (forall r. { bla : Bool, blo : a, ble : a; r } -> a) = fun r => if r.bla then (r.blo + 1) else r.ble
  in (f { bla = true, blo = 1, ble = 2, blip = 'blip } : Number)

