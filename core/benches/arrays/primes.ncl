let range
  | doc "Generate an array of integers in the range [`start`, `end`)."
  | Number -> Number -> Array Number
  = fun start end =>
    if end <= start then
      []
    else
      std.array.generate (fun x => x + start) (end - start)
  in

let is_prime | doc "Returns true if the argument is a prime number."
  = fun x => x > 1 && std.array.all (fun d => x % d != 0) (range 2 (x - 1))
  in

let Prime = std.contract.from_predicate is_prime in

let primes
  | doc "Generate `max` primes using Sieve of Eratosthenes."
  | Number -> Array Prime
  = fun max =>
    let limit = std.number.pow max (1 / 2) in # sqrt(max)
    let drop_multiples = fun x xs =>
      let to_drop =
        max
        |> std.array.generate (fun y => (y + 2) * x)
        |> std.array.filter (fun y => y <= max)
      in
      std.array.filter (fun y => std.array.all ((!=) y) to_drop) xs
    in
    let rec loop = fun x xs =>
      if x > limit then
        xs
      else
        loop (x + 1) (drop_multiples x xs)
    in
    loop 2 (range 2 max)
  in

{
  run = primes
}
