# test.type = 'pass'
let test_cases = {
  "matching type annotation in pattern" = 
    let { a : Number } = { a = 1 } in
    a,

  "matching type annotation in renamed pattern binding" = 
    let { txt : String = other_name } = { txt = "hi" } in
    other_name,

  "matching type annotation in nested pattern" =
    let { a = { b = { c : Bool }}} = { a = { b = { c = true }}} in
    c,

  "matching type annotation above nested pattern" = 
    let { a : { b : { c : Bool }} = { b = { c }}} = { a = { b = { c = false }}} in
    c,
} : _ in
true
