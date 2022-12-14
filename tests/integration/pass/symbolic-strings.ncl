let {check, ..} = import "lib/assert.ncl" in

[
  # Static symbolic string
  s%"hello, world"% == ["hello, world"],
  # Interpolating a string
  let s = "test" in
  s%"This is a %{s}"% == ["This is a ", "test"],
  # Interpolating an interpolated string
  let f = "f" in
  s%"abc %{"de%{f}"}"% == ["abc ", "def"],
  # Interpolating a number
  s%"num: %{100}"% == ["num: ", 100],
  # Interpolating a bool
  s%"bool: %{true}"% == ["bool: ", true],
  # Interpolating an array
  s%"array: %{[true, 1, "yes"]}"% == ["array: ", [true, 1, "yes"]],
  # Interpolating a record
  let r = { a = 1, b = false } in
  s%"record: %{r}"% == ["record: ", r],
  # Interpolating multiple values
  let str = "some string" in
  let num = 999.999 in
  let bool = false in
  let array = ["an", "array", 100] in
  let record = { a = 1, simple = "yes", record = true } in
  let actual = s%"
     1. %{str}
     2. %{num}
     3. %{bool}
     4. %{array}
     5. %{record}"%
  in
  let expected = [
    "1. ", str,
    "\n2. ", num,
    "\n3. ", bool,
    "\n4. ", array,
    "\n5. ", record
  ]
  in
  actual == expected,
]
|> check
