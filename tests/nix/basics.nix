# Basics tests taken from basics.ncl and rewritten with Nix
[
  # basic arithmetic
  (1 + 1 == 2)
  (1 - 2 + 3 - 4 == -2)
  (2 - 3 - 4 == -5)
  (-1 - 2 == -3)
  (2 * 2 + 2 * 3 - 2 * 4 == 2)
  (1 / 2 + 1 / 4 - 1 / 8 == 0.625)
  #((10 + 1/4) % 3 == 1.25)
  #(10 + 1/4 % 3 == 10.25)

  # comparisons
  (1 < 1 == false)
  (1 <= 1 == true)
  (1 > 1 == false)
  (1 >= 1 == true)

  # booleans expr
  (true && false == false)
  (true || false == true)
  (true -> true == true)
  (true -> false == false)
  (false -> true == true)
  (false -> false == true)
  (!false == true)
  (false && true || true == true)
  # (false && (true || true) == false) # TODO: what happen here?

  # lists concataination
  ([ 1 2 ] ++ [ 1 2 ] == [ 1 2 1 2 ])

  # strings concataination
  # ("hello" + " " + "world" == "hello world") # TODO: to be fixed

  # if then else
  ((if true then 1 else 2) == 1)
]
