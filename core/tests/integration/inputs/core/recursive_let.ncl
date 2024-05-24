# test.type = 'pass'


[
  let rec f = fun n => if n == 0 then n else f (n - 1) in f 10 == 0,
  let rec fib = fun n => if n == 0 || n == 1 then 1 else fib (n - 1) + fib (n - 2) in fib 5 == 8,
]
|> std.test.assert_all
