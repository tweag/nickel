# test.type = 'pass'
# eval = 'typecheck'
let x : Number = 1 in
{ 
  include x | String,
  y = x ++ "b",
} : _
