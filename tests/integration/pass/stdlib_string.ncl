let {check, ..} = import "lib/assert.ncl" in

[
  # happy path for all stdlib functions
  # string.BoolLiteral
  ("True" | string.BoolLiteral) == "true",
  ("False" | string.BoolLiteral) == "false",
  ("true" | string.BoolLiteral) == "true",
  ("false" | string.BoolLiteral) == "false",
  # string.NumLiteral
  ("+1.2" | string.NumLiteral) == "+1.2",
  ("-1.2" | string.NumLiteral) == "-1.2",
  ("9001" | string.NumLiteral) == "9001",
  # string.CharLiteral
  ("e" | string.Character) == "e",
  # string.EnumTag
  (`Foo | string.EnumTag) == `Foo,
  # string.Stringable
  (`Foo | string.Stringable) == `Foo,
  (true | string.Stringable) == true,
  (1 | string.Stringable) == 1,
  ("_" | string.NonEmpty) == "_",
  # string.join
  string.join " " ["Hello", "世界"] == "Hello 世界",
  string.join "anything" [] == "",
  string.join "a" ["b", "n", "n", ""] == "banana",
  # string.split
  string.split "," "1,2,3" == ["1", "2", "3"],
  string.split "" "123" == ["", "1", "2", "3", ""],
  string.split "❤" "👩‍❤️‍💋‍👨" == [ "👩‍", "️‍💋‍👨" ],
  string.split "‍" "😶‍🌫️" == ["😶", "🌫️"],
  string.split "" "" == ["", ""],
  # string.trim
  string.trim "  ひげ  " == "ひげ",
  string.trim "   " == "",
  string.trim "" == "",
  string.trim "​ ​ ​ a ​ ​ ​" == "​ ​ ​ a ​ ​ ​", # zero width spaces aren't whitespace
  string.trim "   \n   new\nline\n" == "new\nline",
  string.trim "　   	  " == "",
  # string.characters
  string.characters "漢字" == ["漢", "字"],
  string.characters "👨‍❤️‍💋‍👨" == ["👨", "‍", "❤", "️", "‍", "💋", "‍", "👨"], # arguably wrong, splits the ZWJ combined emoji
  string.characters "ab_cd" == ["a", "b", "_", "c", "d"],
  string.characters "" == [],
  # string.code
  string.codepoint "A" == 65,
  string.codepoint "%" == 37,
  # string.from_code
  string.from_codepoint 65 == "A",
  string.from_codepoint 37 == "%",
  # code/from_code round trip
  (array.all (fun code => (string.codepoint (string.from_codepoint code)) == code) (array.generate function.id 127)),
  # string.uppercase
  string.uppercase "abcd" == "ABCD",
  string.uppercase "子供" == "子供",
  string.uppercase "アαφ" == "アΑΦ",
  string.uppercase "" == "",
  # string.lowercase
  string.lowercase "ABCD" == "abcd",
  string.lowercase "子供" == "子供",
  string.lowercase "アΑΦ" == "アαφ",
  string.lowercase "" == "",
  # string.contains
  string.contains "" "",
  string.contains "_" "__",
  string.contains "\n" "\n\n\n",
  !(string.contains "\n" "\r"),
  !(string.contains "_" "no underscores"),
  !(string.contains "👨‍❤️‍💋‍👨" "❤"),
  # string.replace
  string.replace "" "a" "bnn" == "abanana",
  string.replace "the present" "the future" "the present is now" == "the future is now",
  string.replace "nickel" "iron" "iron man" == "iron man",
  # string.replace_regex
  string.replace_regex "\\d" "_" "1,234,1 1" == "_,___,_ _",
  string.replace_regex "\\d+" "_" "1,234,1 1" == "_,_,_ _",
  string.replace_regex "\\d" "_" "１２３" == "___",
  string.replace_regex "\\d" "_" "一二三" == "一二三",
  # string.is_match
  string.is_match "^___$" "___",
  string.is_match "___" "___)",
  !(string.is_match "^___$" "___)"),
  # string.find
  string.find "([0-9]{1,3}\\.){3}([0-9]{1,3})" "1.2.3.4" == { matched = "1.2.3.4", index = 0, groups = ["3.", "4"]},
  string.find "([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})" "ip: 192.168.1.4, sorry, what's ipv6?" == { matched = "192.168.1.4", index = 4, groups = ["192", "168", "1", "4"]},
  string.find "\\d" "no numeral" == { matched = "", index = -1, groups = []},

  # string.length
  string.length "" == 0,
  string.length " " == 1,
  string.length "nickel" == 6,
  # Hungarian `ő` character.
  string.length "unicőde" == 7,
  # Vietnamese `ế ` character.
  string.length "nickếl" == 6,
  string.length "四字熟語" == 4,
  string.length "👩🏿‍❤️‍💋‍👩🏼" == 1,
  string.length "👨‍❤️‍💋‍👨" == 1,

  # string.substring
  string.substring 3 5 "abcdef" == "de",
  # string.from
  string.from true == "true",
  string.from 1 == "1",
  string.from "asdf" == "asdf",
  string.from `Foo == "Foo",
  # string.from_number
  string.from_number (-1) == "-1",
  string.from_number 1 == "1",
  string.from_number 0 == "0",
  string.from_number 9009 == "9009",
  string.from_number 1.2 == "1.2",
  # string.from_enum
  string.from_enum `Enum == "Enum",
  # string.to_number
  string.to_number "1" == 1,
  string.to_number "-1" == (-1),
  string.to_number "-1.1" == (-1.1),
  # string.to_bool
  string.to_bool "True",
  string.to_bool "true",
  string.to_bool "false" == false,
  string.to_bool "False" == false,
  # string.to_enum
  string.to_enum "" == `"",
  string.to_enum "x" == `x,
  string.to_enum "X" == `X,
  string.to_enum "X" == string.to_enum "X",
  string.to_enum "タグ" == `"タグ",
]
|> check
