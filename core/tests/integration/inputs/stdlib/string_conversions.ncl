# test.type = 'pass'

let {string, ..} = std in

[
  # string.from
  string.from true == "true",
  string.from 1 == "1",
  string.from "asdf" == "asdf",
  string.from 'Foo == "Foo",

  # string.from_number
  string.from_number (-1) == "-1",
  string.from_number 1 == "1",
  string.from_number 0 == "0",
  string.from_number 9009 == "9009",
  string.from_number 1.2 == "1.2",

  # string.from_enum
  string.from_enum 'Enum == "Enum",
  string.from_enum 'Magick == "Magick",

  # string.to_number
  string.to_number "1" == 1,
  string.to_number "-1" == (-1),
  string.to_number "-1.1" == (-1.1),

  # string.to_bool
  string.to_bool "true",
  !(string.to_bool "false"),

  # string.to_enum
  string.to_enum "" == '"",
  string.to_enum "x" == 'x,
  string.to_enum "X" == 'X,
  string.to_enum "X" == string.to_enum "X",
  string.to_enum "タグ" == '"タグ",
] |> std.test.assert_all
