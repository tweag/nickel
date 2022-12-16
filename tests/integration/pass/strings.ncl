let {check, ..} = import "lib/assert.ncl" in

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
  m%""%{"foo"}""% == "\"foo\"",
  m%"""% == "\"",

  # regression test for issue #596 (https://github.com/tweag/nickel/issues/596)
  let s = "Hello" in m%%""%%{s}" World"%% == "\"Hello\" World",
  let s = "Hello" in m%%""%%%{s}" World"%% == "\"%Hello\" World",
  m%"%s"% == "%s",
  m%%"%%s"%% == "%%s",

  # regression test for issue #659 (https://github.com/tweag/nickel/issues/659)
  let b = "x" in m%"a%%{b}c"% == "a%xc",
  m%"%Hel%%{"1"}lo%%%{"2"}"% == "%Hel%1lo%%2",
  let res = string.find "a" "bac" in res.matched == "a" && res.index == 1,

  # length
  string.length "" == 0,
  string.length "nickel" == 6,
  # Hungarian `ő` character.
  string.length "unicőde" == 7,
  # Vietnamese `ế ` character.
  string.length "nickếl" == 6,
  string.length "四字熟語" == 4,
  string.length "👩🏿‍❤️‍💋‍👩🏼" == 1,
  string.length "👨‍❤️‍💋‍👨" == 1,
]
|> check
