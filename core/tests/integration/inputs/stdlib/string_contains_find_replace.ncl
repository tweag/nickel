# test.type = 'pass'
let {check, ..} = import "../lib/assert.ncl" in
let {string, ..} = std in

[
  # string.contains
  string.contains "" "",
  string.contains "_" "__",
  string.contains "\n" "\n\n\n",
  !(string.contains "\n" "\r"),
  !(string.contains "_" "no underscores"),
  !(string.contains "❤" "👨‍❤️‍💋‍👨"),
  !(string.contains "words👨" "words👨‍❤️‍💋‍👨"),

  # string.replace
  string.replace "" "a" "bnn" == "abanana",
  string.replace "the present" "the future" "the present is now" == "the future is now",
  string.replace "iron" "nickel" "iron man" == "nickel man",
  string.replace "nickel" "iron" "iron man" == "iron man",
  string.replace "❤" "egg" "👨‍❤️‍💋‍👨" == "👨‍❤️‍💋‍👨",
  string.replace "has" "had" "this has multiple matches, has it?" == "this had multiple matches, had it?",
  string.replace "hello👨" "goodbye" "hello👨‍❤️‍💋‍👨" == "hello👨‍❤️‍💋‍👨",

  # string.replace_regex
  string.replace_regex "\\d" "_" "1,234,1 1" == "_,___,_ _",
  string.replace_regex "\\d+" "_" "1,234,1 1" == "_,_,_ _",
  string.replace_regex "\\d" "_" "１２３" == "___",
  string.replace_regex "\\d" "_" "一二三" == "一二三",
  # we don't want to replace codepoints inside extended grapheme clusters
  string.replace_regex "❤️" "_" "👨‍❤️‍💋‍👨" == "👨‍❤️‍💋‍👨",
  string.replace_regex "❤️" "_" "x❤️y❤️z" == "x_y_z",

  # string.is_match
  string.is_match "^___$" "___",
  string.is_match "___" "___)",
  !(string.is_match "^___$" "___)"),
  # we don't want matches if they occur as part of an extended grapheme cluster
  !(string.is_match "👨" "👨‍❤️‍💋‍👨"),
  !(string.is_match "❤️" "👨‍❤️‍💋‍👨"),

  # string.find
  string.find "([0-9]{1,3}\\.){3}([0-9]{1,3})" "1.2.3.4" == { matched = "1.2.3.4", index = 0, groups = ["3.", "4"]},
  string.find "([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})" "ip: 192.168.1.4, sorry, what's ipv6?" == { matched = "192.168.1.4", index = 4, groups = ["192", "168", "1", "4"]},
  string.find "\\d" "no numeral" == { matched = "", index = -1, groups = []},
  string.find "❤️" "👨‍❤️‍💋‍👨" == { matched = "", index = -1, groups = []},
  string.find "❤️" "👨‍❤️‍💋‍👨❤️" == { matched = "❤️", index = 1, groups = [] },
  ] |> check
