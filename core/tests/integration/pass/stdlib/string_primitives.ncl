# test.type = 'pass'
let {check, ..} = import "../lib/assert.ncl" in
let {string, ..} = std in

[
  # string.characters
  string.characters "漢字" == ["漢", "字"],
  string.characters "👨‍❤️‍💋‍👨" == ["👨‍❤️‍💋‍👨"],
  string.characters "ab_cd" == ["a", "b", "_", "c", "d"],
  string.characters "" == [],
  string.characters "😭👻👀" == ["😭", "👻", "👀"],
  string.characters "👩🏾‍🦲👩🏻‍🦲👩🏿‍🦲" == ["👩🏾‍🦲", "👩🏻‍🦲", "👩🏿‍🦲"],
  string.characters "unicőde" == ["u", "n", "i", "c", "ő", "d", "e"],

  # string.length
  string.length "" == 0,
  string.length " " == 1,
  string.length "nickel" == 6,
  string.length m%"
      a multiline string
      stretched over lines
      what length should it be?"% == 65,
  # Hungarian `ő` character.
  string.length "unicőde" == 7,
  # Vietnamese `ế ` character.
  string.length "nickếl" == 6,
  string.length "四字熟語" == 4,
  string.length "👩🏿‍❤️‍💋‍👩🏼" == 1,
  string.length "👨‍❤️‍💋‍👨" == 1,

  # string.substring
  string.substring 3 5 "abcdef" == "de",
  string.substring 0 6 "nickel is great" == "nickel",
  string.substring 0 1 "👩🏻‍🦲" == "👩🏻‍🦲",
  string.substring 1 6 "👨‍❤️‍💋‍👨hello" == "hello",
  string.substring 2 4 "ab👨‍❤️‍💋‍👨👨‍❤️‍💋‍👨cd" == "👨‍❤️‍💋‍👨👨‍❤️‍💋‍👨",
] |> check
