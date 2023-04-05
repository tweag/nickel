[
  (let a = "a"; in a == "a")
  (let a = "a"; b = "b"; in [ a b ] == [ "a" "b" ])
  (let a = c; b = a + c; c = 1; in [ a b c ] == [ 1 2 1 ])
  (let a = 1; in let b = a; in a + b == 2)
]
