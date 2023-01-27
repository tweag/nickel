# has_field operator (`?`)
[
  ({ a = 1; } ? a == true)
  ({ a = 1; } ? "a" == true)
  ({ a = 1; } ? b == false)
  ({ a = 1; } ? "b" == false)
  ({ a.foo = 1; } ? a.foo == true)
  ({ a.foo = 1; } ? a."foo" == true)
  ({ a.foo = 1; } ? "a.foo" == false)
  ({ a.foo = 1; } ? "a".foo == true)
  ({ a.foo = 1; } ? a == true)

  # field access or default
  ({ a = "a"; }.a or "x" == "a")
  ({ a = "a"; }.b or "x" == "x")
  ({ a.b = "ab"; }.a or "x" == { b = "ab"; })
  ({ a.b = "ab"; }.a.b or "x" == "ab")
  ({ a.b = "ab"; }.a.c or "x" == "x")
]
