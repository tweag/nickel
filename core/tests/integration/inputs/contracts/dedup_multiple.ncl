# test.type = 'pass'

# Regression test for https://github.com/tweag/nickel/issues/2189
let Linktype = {
  source,
  target
    | Dyn
    | { file | default = source }
}
in
(
  {
    source = "",
    target | Dyn | Dyn = {},
  } | Linktype
) == {
  source = "",
  target.file = "",
}
