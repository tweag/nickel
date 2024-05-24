# test.type = 'pass'


[
  (fun x => x) 3 == 3,
  (fun x y => x) 1 2 == 1,
  (fun x y => y) 1 2 == 2,
  (fun f x => f 0) (fun x => x+2) 2 == 2,

  let f = fun f y => f (f y) in f (fun x => x+1) 3 == 5,
  let g  = fun x => if x then 0 else false in g ((fun x => true) 23)
    == 0,
  # Y fixpont combinator
  let Y = (fun f => (fun x => f (x x)) (fun x => f (x x))) in
    let g = Y (fun g => (fun x => if x  then (g false) else 4)) in
    g true
    == 4,
]
|> std.test.assert_all
