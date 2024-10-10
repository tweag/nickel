let rec sum | Array Number -> Number
  = match {
    [] => 0,
    [x, ..xs] => x + sum xs,
  }
  in
sum [1, 2, 3]
