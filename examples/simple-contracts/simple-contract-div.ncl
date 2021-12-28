// /!\ THIS EXAMPLE IS EXPECTED TO FAIL
// Illustrate a basic contract violation.

let Even = fun label value =>
  if builtins.is_num value && value % 2 == 0 then
    value
  else
    contracts.blame label in
let DivBy3 = fun label value =>
  if builtins.is_num value && value % 3 == 0 then
    value
  else
    contracts.blame label in
// Will cause an error! 4 is no divided by 3.
(4 | #Even
   | #DivBy3)
