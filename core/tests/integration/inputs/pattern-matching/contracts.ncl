# test.type = 'pass'



[
  {foo = {}} |> match { {foo = {bar ? 5}} => true},
  {foo = {}} |> match { {foo = {bar | String }} => false, {foo} => true},
]
|> std.test.assert_all
