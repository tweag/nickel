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
]

|> lists.foldl (fun x y => (x | #Assert) && y) true
