# test.type = 'pass'

let sym = fun prefix_ fragments_ => { tag = 'SymbolicString, prefix = prefix_, fragments = fragments_ } in

[
  # Static symbolic string
  foo-s%"hello, world"% == sym 'foo ["hello, world"] ,
  # Interpolating a string
  let s = "test" in
  foo-s%"This is a %{s}"% == sym 'foo ["This is a ", "test"],
  # Interpolating an interpolated string
  let f = "f" in
  foo-s%"abc %{"de%{f}"}"% == sym 'foo ["abc ", "def"],
  # Interpolating a number
  foo-s%"num: %{100}"% == sym 'foo ["num: ", 100],
  # Interpolating a bool
  foo-s%"bool: %{true}"% == sym 'foo ["bool: ", true],
  # Interpolating an array
  foo-s%"array: %{[true, 1, "yes"]}"% == sym 'foo ["array: ", [true, 1, "yes"]],
  # Interpolating a record
  let r = { a = 1, b = false } in
  foo-s%"record: %{r}"% == sym 'foo ["record: ", r],
  # Interpolating multiple values
  let str = "some string" in
  let num = 999.999 in
  let bool = false in
  let array = ["an", "array", 100] in
  let record = { a = 1, simple = "yes", record = true } in
  let actual = foo-s%"
     1. %{str}
     2. %{num}
     3. %{bool}
     4. %{array}
     5. %{record}"%
  in
  let expected = sym 'foo [
    "1. ", str,
    "\n2. ", num,
    "\n3. ", bool,
    "\n4. ", array,
    "\n5. ", record
  ]
  in
  actual == expected,
]
|> std.test.assert_all
