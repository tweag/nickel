let foo = {
  a = 1,
  b.d = 2,
  c = 3,
} in
foo |> matchv2 {
  {a, b = {d = d'}, ..rest} => d' + a + rest,
  {a, b = {d}} => d + a + 10,
}
