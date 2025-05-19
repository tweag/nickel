# test.type = 'pass'
# eval = 'typecheck'

# Test that the `SomeContract` opaque type affected to `x` is properly coming
# from the outer environment, and isn't polluted by the local recursive
# re-definition of `SomeContract` to `Number`.
let SomeContract = String in
let x | SomeContract = "hello" in
{
  include x,
  SomeContract = Number,
} : {x : SomeContract, SomeContract : _}
