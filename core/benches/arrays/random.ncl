let mgc | doc "A multiplicative congruential pseudorandom number generator (MCG)."
  = fun params x => (params.a * x + params.c) % params.m
  in

# NOTE: These parameters are from "Table 7: Good multipliers for MCGs with m = 2^32"
# of the 2021 paper "Computationally Easy, Spectrally Good Multipliers for
# Congruential Pseudorandom Number Generators", by Guy Steele and Sebastiano Vigna.
# But, they cannot be used because they require bit-shifting to select the 32 MSBs.
# let params = { m = number.pow 2 32, a = 2480367069 } in

# These parameters are from the "Numerical Recipes" book.
let params = { m = std.number.pow 2 32, a = 1664525, c = 1013904223 } in

let rec array_random = fun init n =>
  if n == 0 then
    [init]
  else
    let next = mgc params init in
    [next] @ array_random next (n - 1)
in

{ run = array_random 42 }
