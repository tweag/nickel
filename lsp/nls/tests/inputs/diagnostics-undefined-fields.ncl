### /diagnostics-undefined-fields.ncl
{
  x | Number,
  y = x,
  z = x, # Second reference to the same field, to check that the thunk is reset on the first error.
}
### # We don't issue diagnostics on undefined fields, because they're expected for partial configurations.
### # Even if undefined fields are referenced, that isn't an error.
### diagnostic = ["file:///diagnostics-undefined-fields.ncl"]
