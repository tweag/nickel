# test = 'blame'

# /!\ THIS EXAMPLE IS EXPECTED TO FAIL
# Illustrates a basic contract violation.
let Even = fun label value =>
  if std.is_number value && value % 2 == 0 then
    value
  else
    std.contract.blame label
in
let DivBy3 = fun label value =>
  if std.is_number value && value % 3 == 0 then
    value
  else
    std.contract.blame label
in
# Will cause an error! 4 is not divisible by 3.
(
  4
    | Even
    | DivBy3
)
