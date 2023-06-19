# test.type = 'pass'
let {check, ..} = import "../lib/assert.ncl" in
let {string, ..} = std in

[
  # string.join
  string.join " " ["Hello", "世界"] == "Hello 世界",
  string.join "anything" [] == "",
  string.join "a" ["b", "n", "n", ""] == "banana",

  # string.split
  string.split "," "1,2,3" == ["1", "2", "3"],
  string.split "" "123" == ["1", "2", "3"],
  string.split "❤" "👩‍❤️‍💋‍👨" == ["👩‍❤️‍💋‍👨"],
  string.split "‍" "😶‍🌫️" == ["😶‍🌫️"],
  string.split "😶‍🌫️" "👩‍❤️‍💋‍👨😶‍🌫️👩‍❤️‍💋‍👨" == ["👩‍❤️‍💋‍👨", "👩‍❤️‍💋‍👨"],
  string.split "" "" == [],
  string.split "x" "abc" == ["abc"],
  string.split "❤" "i❤u" == ["i", "u"],
  string.split "abc" "xabcyabcz" == ["x", "y", "z"],
  string.split "❤❤" "x❤❤y❤z" == ["x", "y❤z"],
  string.split "a" "abacus" == ["", "b", "cus"],
  string.split "a" "banana" == ["b", "n", "n"],
  string.split "a👨" "a👨‍❤️‍💋‍👨" == ["a👨‍❤️‍💋‍👨"],

  # string.split/string.join roundtrip
  std.array.all (fun {sep, str} => str == string.join sep (string.split sep str)) [
    { sep = ",", str = "a,b,c,d,e,f,g" },
    { sep = ":", str = "1:2:3:4:5" },
    { sep = "--", str = "yes--no--maybe--x--y--z" },
    { sep = " ", str = "this is a sentence."},
    { sep = "👩‍❤️‍💋‍👨", str = "-👩‍❤️‍💋‍👨👩‍❤️‍💋‍👨👩‍❤️‍💋‍👨-" },
  ],
] |> check
