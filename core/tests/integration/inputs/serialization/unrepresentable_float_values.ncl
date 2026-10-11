# test.type = 'pass'

let expected = {
  plus_inf = 'PlusInfinity,
  minus_inf = 'MinusInfinity,
  not_a_number = 'NotANumber,
}
in

[
  {
    format = 'Yaml,
    data = m%"
      {
        plus_inf: .inf,
        minus_inf: -.INF,
        not_a_number: .nan,
      }
    "%,
  },
  {
    format = 'Yaml,
    data = m%"
        {
          plus_inf: +.INF,
          minus_inf: -.inf,
          not_a_number: .nan,
        }
      "%,
  },
  {
    format = 'Toml,
    data = m%"
      plus_inf = inf
      minus_inf = -inf
      not_a_number = nan
    "%,
  },
  {
    format = 'Toml,
    data = m%"
      plus_inf = +inf
      minus_inf = -inf
      not_a_number = +nan
    "%,
  },
  {
    format = 'Toml,
    data = m%"
      plus_inf = +inf
      minus_inf = -inf
      not_a_number = -nan
    "%,
  }
]
|> std.array.map (fun { format, data } => std.deserialize format data == expected)
|> std.test.assert_all
