// Currently, only record bindings are recursive. To use a recursive function,
// one has to use a record.

// This is the naive, exponential version of fibonacci: don't call it on a big
// value!
let fibonacci = {
 f = fun n =>
  if n == 0 then
    0 
  else if n == 1 then
    1
  else 
    f (n-1) + f (n-2)
}.f in
fibonacci 10
