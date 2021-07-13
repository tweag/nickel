let Assert = fun l x => x || %blame% l in

// interpolation
("simple #{"interp" ++ "olation"} here" == "simple interpolation here"
  | #Assert) &&
("#{"alone"}" == "alone" | #Assert) &&
("nested #{ "#{(fun x => "#{x}") "expression"}" }"
  == "nested expression" | #Assert) &&
("#{"some"}#{" " ++ "se" ++ "qu"}#{"#{"ence"}"}"
  == "some sequence" | #Assert) &&
("nested #{ {str = {a = "braces"}.a}.str } !"
  == "nested braces !" | #Assert) &&
((let x = "world" in
  "Hello, #{x}! Welcome in #{let y = "universe" in "the #{x}-#{y}"}")
  == "Hello, world! Welcome in the world-universe" | #Assert) &&

// regression test for issue #361 (https://github.com/tweag/nickel/issues/361)
(m#""#{"foo"}""#m == "\"foo\"" | #Assert) &&
(m#"""#m == "\"" | #Assert) &&
(m#""#"#"#"#m == "\"#\"#\"#" | #Assert) &&

true

