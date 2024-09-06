# test = 'blame'

# /!\ THIS EXAMPLE IS EXPECTED TO FAIL
# Illustrates a basic contract violation.
let Even =
  std.contract.from_predicate (fun value =>
    std.is_number value && value % 2 == 0
  )
in

let DivBy3 =
  std.contract.from_predicate (fun value =>
    std.is_number value && value % 3 == 0
  )
in

# Will cause an error! 4 is not divisible by 3.
(
  4
    | Even
    | DivBy3
)
