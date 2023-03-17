let {check, ..} = import "lib/assert.ncl" in

[
  # string.codepoint
  string.codepoint "A" == 65,
  string.codepoint "%" == 37,

  # string.from_codepoint
  string.from_codepoint 65 == "A",
  string.from_codepoint 37 == "%",

  # string.codepoint/string.from_codepoint round trip
  (array.all (fun code => (string.codepoint (string.from_codepoint code)) == code) (array.generate function.id 127)),

  # string.from_codepoint/string.codepoint roundtrip
  array.all (fun char => char == string.from_codepoint (string.codepoint char)) [
    "a",
    "e",
    "i",
    "o",
    "u",
    "%",
    "!",
    "@",
    "9",
    "3",
    "X",
    "M",
    "L",
    "H",
  ],
] |> check