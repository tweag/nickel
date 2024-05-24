# test.type = 'pass'

let {string, ..} = std in

[
    # string.uppercase
  string.uppercase "abcd" == "ABCD",
  string.uppercase "子供" == "子供",
  string.uppercase "アαφ" == "アΑΦ",
  string.uppercase "" == "",
  string.uppercase "👨‍❤️‍💋‍👨" == "👨‍❤️‍💋‍👨",

  # string.lowercase
  string.lowercase "ABCD" == "abcd",
  string.lowercase "子供" == "子供",
  string.lowercase "アΑΦ" == "アαφ",
  string.lowercase "" == "",
  string.lowercase "👨‍❤️‍💋‍👨" == "👨‍❤️‍💋‍👨",

  # string.uppercase/string.lowercase roundtrip
  std.array.all (fun s => s == string.lowercase (string.uppercase s)) [
    "abacus",
    "abcd",
    "xxxxyyyyzzzz",
    "this is a test"
  ],

  # string.lowercase/string.uppercase roundtrip
  std.array.all (fun s => s == string.uppercase (string.lowercase s)) [
    "ABACUS",
    "ABCD",
    "XXXXYYYYZZZZ",
    "THIS IS A TEST"
  ],
] |> std.test.assert_all
