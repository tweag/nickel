# test.type = 'pass'


[
  %enum/unwrap_variant% ('Left (1+1)) == 2,
  !(%enum/is_variant% 'Right),
  %enum/is_variant% ('Right 1),
  %enum/get_tag% 'Right == 'Right,
  %enum/get_tag% ('Right "stuff") == 'Right,
]
|> std.test.assert_all
