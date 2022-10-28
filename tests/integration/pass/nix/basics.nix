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
]
