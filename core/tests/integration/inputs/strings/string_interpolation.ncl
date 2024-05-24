# test.type = 'pass'


[
  # Simple interpolation tests
  let actual = "simple %{"interp" ++ "olation"} here" in
  let expected = "simple interpolation here" in
  actual == expected,

  "%{"alone"}" == "alone",

  let actual = "nested %{ "%{(fun x => "%{x}") "expression" }" }" in
  let expected = "nested expression" in
  actual == expected,

  let actual = "%{"some"}%{" " ++ "se" ++ "qu"}%{"%{"ence"}"}" in
  let expected = "some sequence" in
  actual == expected,

  let actual = "nested %{ {str = {a = "braces"}.a}.str } !" in
  let expected = "nested braces !" in
  actual == expected,

  let actual = (let x = "world" in "Hello, %{x}! Welcome in %{let y = "universe" in "the %{x}-%{y}"}") in
  let expected = "Hello, world! Welcome in the world-universe" in
  actual == expected,

  # Regression tests for  [#361](https://github.com/tweag/nickel/issues/361)
  m%""%{"foo"}""% == "\"foo\"",
  m%"""% == "\"",

  # Regression tests for [#596](https://github.com/tweag/nickel/issues/596)
  let s = "Hello" in m%%""%%{s}" World"%% == "\"Hello\" World",
  let s = "Hello" in m%%""%%%{s}" World"%% == "\"%Hello\" World",
  m%"%s"% == "%s",
  m%%"%%s"%% == "%%s",

  # Regression tests for [#659](https://github.com/tweag/nickel/issues/659)
  let b = "x" in m%"a%%{b}c"% == "a%xc",
  m%"%Hel%%{"1"}lo%%%{"2"}"% == "%Hel%1lo%%2",

  # Regression tests for [#987](https://github.com/tweag/nickel/issues/987)
  let res = std.string.find "a" "bac" in res.matched == "a" && res.index == 1,

  # Regression tests for [#1012](https://github.com/tweag/nickel/issues/1012)
  let actual = m%%"Some text s%"%{not-interpolated}"% more text"%% in
  let expected = "Some text s\%\"\%{not-interpolated}\"\% more text" in
  actual == expected,

  let actual = m%%%"ABC %%%%{"test"}"%%% in
  let expected = "ABC %test" in
  actual == expected,

  let actual = m%%"XYZ "%%%{"CBA"}""%% in
  let expected = "XYZ \"%CBA\"" in
  actual == expected,
] |> std.test.assert_all
