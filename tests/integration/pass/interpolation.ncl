let {check, ..} = import "lib/assert.ncl" in

[
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
] |> check
