# Note: currently repeated identifiers in patterns aren't caught unless
#       we're in typechecking mode. there's an open issue for this (#1098)
(
  let { a, a, .. } = { a = 1, b = 2 } in
  a : Number
): _
