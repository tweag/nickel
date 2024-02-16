# test.type = 'pass'

let {check, ..} = import "../lib/assert.ncl" in

[
  {foo = {}} |> match { {foo = {bar ? 5}} => true},
  {foo = {}} |> match { {foo = {bar | String }} => false, {foo} => true},
]
|> check
