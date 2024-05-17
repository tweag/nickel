# test = 'pass'

# This is the naive, exponential version of fibonacci: don't call it on a large
# number!
let rec fibonacci = match {
  0 => 0,
  1 => 1,
  n => fibonacci (n - 1) + fibonacci (n - 2),
}
in
fibonacci 10
