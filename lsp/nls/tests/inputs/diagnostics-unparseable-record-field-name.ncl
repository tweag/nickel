### /diagnostics.ncl
{
  # The parser inserts an extra field into this record due to the duplicate comma,
  # but it should be ignored for typechecking purposes. Diagnostics should only include the
  # parser error.
  x = 1,,
  y = 2
} : { x : Number, y : Number }
### diagnostic = ["file:///diagnostics.ncl"]
