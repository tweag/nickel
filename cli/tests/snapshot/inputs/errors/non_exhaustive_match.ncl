# capture = 'stderr'
# command = ['eval']

# Ensure that a non-exhaustive pattern matching applied to a non-matching
# argument produces a proper error message.
let x = if true then 'a else 'b in
let f = match {
  'c => "hello",
  'd => "adios",
}
in

f x
