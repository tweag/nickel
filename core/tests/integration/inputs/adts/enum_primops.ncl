# test.type = 'pass'
let {check, ..} = import "../lib/assert.ncl" in

[
  %enum_unwrap_variant% ('Left (1+1)) == 2,
  !(%enum_is_variant% 'Right),
  %enum_is_variant% ('Right 1),
  %enum_get_tag% 'Right == 'Right,
  %enum_get_tag% ('Right "stuff") == 'Right,
]
|> check
