### /diagnostics.ncl
{
  x = 1,
  # A parser error exists here, but the typechecking error is useful in this case, so we want
  # to make sure the typechecking diagnostic is still published.
  z = ,
  y = 2
} : { x : Number, y : Number }
### diagnostic = ["file:///diagnostics.ncl"]
