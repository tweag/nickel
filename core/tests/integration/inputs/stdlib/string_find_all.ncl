# test.type = 'pass'

let { string, .. } = std in

[
  std.string.find_all "a" "aaa bbb ccc abc" == [
    { groups = [], index = 0, matched = "a", },
    { groups = [], index = 1, matched = "a", },
    { groups = [], index = 2, matched = "a", },
    { groups = [], index = 12, matched = "a", }
  ],
  std.string.find_all "([a-z]+)=([0-9]+)" "one=1, two=2, three=3" == [
    { groups = ["one", "1"], index = 0, matched = "one=1", },
    { groups = ["two", "2"], index = 7, matched = "two=2", },
    { groups = ["three", "3"], index = 14, matched = "three=3", }
  ],
  std.string.find_all "(\\d+)\\.(\\d+)\\.(\\d+)" "1.2.3; 4.5.6; 7.8.9; 10.11.12; 13.14.15" == [
    { groups = ["1", "2", "3"], index = 0, matched = "1.2.3", },
    { groups = ["4", "5", "6"], index = 7, matched = "4.5.6", },
    { groups = ["7", "8", "9"], index = 14, matched = "7.8.9", },
    { groups = ["10", "11", "12"], index = 21, matched = "10.11.12", },
    { groups = ["13", "14", "15"], index = 31, matched = "13.14.15", }
  ],
  std.string.find_all "(\\p{Emoji})=(\\w+)" "😀=smiling, 🤗=hugging, 🫠=melting" == [
    { groups = ["😀", "smiling"], index = 0, matched = "😀=smiling", },
    { groups = ["🤗", "hugging"], index = 11, matched = "🤗=hugging", },
    { groups = ["🫠", "melting"], index = 22, matched = "🫠=melting", }
  ]
]
|> std.test.assert_all
