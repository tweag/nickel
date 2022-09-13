let {check, ..} = import "testlib.ncl" in

[
  # interpolation
  "simple %{"interp" ++ "olation"} here" == "simple interpolation here",
  "%{"alone"}" == "alone",
  "nested %{ "%{(fun x => "%{x}") "expression"}" }" == "nested expression",
  "%{"some"}%{" " ++ "se" ++ "qu"}%{"%{"ence"}"}" == "some sequence",
  "nested %{ {str = {a = "braces"}.a}.str } !" == "nested braces !",
  (let x = "world" in
    "Hello, %{x}! Welcome in %{let y = "universe" in "the %{x}-%{y}"}")
    == "Hello, world! Welcome in the world-universe",

  # regression test for issue #361 (https://github.com/tweag/nickel/issues/361)
  m%""%{"foo"}""%m == "\"foo\"",
  m%"""%m == "\"",
  m%""%"%"%"%m == "\"%\"%\"%",

  # regression test for issue #596 (https://github.com/tweag/nickel/issues/596)
  let s = "Hello" in m%%""%%{s}" World"%%m == "\"Hello\" World",
  let s = "Hello" in m%%""%%%{s}" World"%%m == "\"%Hello\" World",
  m%%""%%s"%%m == "\"%%s",

  # regression test for issue #659 (https://github.com/tweag/nickel/issues/659)
  let b = "x" in m%"a%%{b}c"%m == "a%xc",
  m%"%Hel%%{"1"}lo%%%{"2"}"%m == "%Hel%1lo%%2",
]
|> check
