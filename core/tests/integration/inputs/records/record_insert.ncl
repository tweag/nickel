# test.type = 'pass'

# regression test for record insertion and record removal not being consistent
# with respect to optional fields
let my_insert = fun field content r =>
  let r =
    if %record/has_field% field r then
      %record/remove% field r
    else
      r
  in
  %record/insert% field r content
in
let foo | { value | optional, .. } = { bar = "hello" } in
my_insert "value" "world" foo == { bar = "hello", value = "world" }
