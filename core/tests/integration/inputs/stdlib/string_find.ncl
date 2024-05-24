# test.type = 'pass'

let { string, .. } = std in

[
  std.string.find "a" "aaa bbb ccc abc" == { groups = [], index = 0, matched = "a" },
  std.string.find "([a-z]+)=([0-9]+)" "one=1, two=2, three=3" == { groups = ["one", "1"], index = 0, matched = "one=1" },
  std.string.find "(\\d+)\\.(\\d+)\\.(\\d+)" "1.2.3" == { groups = ["1", "2", "3"], index = 0, matched = "1.2.3" },
  std.string.find "(\\p{Emoji})=(\\w+)" "😀=smiling" == { groups = ["😀", "smiling"], index = 0, matched = "😀=smiling" }
]
|> std.test.assert_all
