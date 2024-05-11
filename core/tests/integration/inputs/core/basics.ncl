# test.type = 'pass'


[
  # basic arithmetic
  1+1 == 2,
  1-2+3-4 == -2,
  2-3-4 == -5,
  -1-2 == -3,
  2*2 + 2*3 - 2*4 == 2,
  1/2 + 1/4 - 1/8 == 0.625,
  (10 + 1/4) % 3 == 1.25,
  10 + 1/4 % 3 == 10.25,
  34 + (if true then 2 else 222)
    == 36,

  # comparisons
  1 < 1 == false,
  1 <= 1 == true,
  1 > 1 == false,
  1 >= 1 == true,
  1 + 1/2 > 1 + 1/4 == true,
  1 + 1/2 < 1 + 1/4 == false,
  1 + 1/2 + 1/8 > 1 + 1/4 + 1/4 == true,
  1 + 1/2 + 1/8 < 1 + 1/4 + 1/4 == false,
  -1 - 2 < 3 - 10 == false,
  -1 - 2 > 3 - 10 == true,
  -1*2 > 1*2 == false,
  -1*2 < 1*2 == true,
  1/4 + 1/4 - 1/4 + 1/4 <= 1/2 == true,
  1/4 + 1/4 - 1/4 + 1/4 < 1/2 == false,
  1/4 + 1/4 - 1/4 + 1/4 >= 1/2 == true,
  1/4 + 1/4 - 1/4 + 1/4 < 1/2 == false,

  # This test checks that the terms of a match are closured
  let x = 3 in ((3 + 2) |> match { 'foo => 1, _ => x}) == 3,
]
|> std.test.assert_all
