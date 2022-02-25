let Assert = fun l x => x || %blame% l in

[
  // interpolation
  "simple #{"interp" ++ "olation"} here" == "simple interpolation here",
  "#{"alone"}" == "alone",
  "nested #{ "#{(fun x => "#{x}") "expression"}" }" == "nested expression",
  "#{"some"}#{" " ++ "se" ++ "qu"}#{"#{"ence"}"}" == "some sequence",
  "nested #{ {str = {a = "braces"}.a}.str } !" == "nested braces !",
  (let x = "world" in
    "Hello, #{x}! Welcome in #{let y = "universe" in "the #{x}-#{y}"}")
    == "Hello, world! Welcome in the world-universe",

  // regression test for issue #361 (https://github.com/tweag/nickel/issues/361)
  m#""#{"foo"}""#m == "\"foo\"",
  m#"""#m == "\"",
  m#""#"#"#"#m == "\"#\"#\"#",

  // regression test for issue #596 (https://github.com/tweag/nickel/issues/596)
  let s = "Hello" in m##""##{s}" World"##m == "\"Hello\" World",
  let s = "Hello" in m##""###{s}" World"##m == "\"#Hello\" World",
  m##""##s"##m == "\"##s",
]
|> list.foldl (fun x y => (x | Assert) && y) true
