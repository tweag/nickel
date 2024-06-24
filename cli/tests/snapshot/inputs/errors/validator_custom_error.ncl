# capture = 'stderr'
# command = ['eval']
let Is42 = std.contract.from_validator (fun value =>
  if value == 42 then
    'Ok value
  else
    'Error {
      message = "Value must be 42",
      notes = ["This is a first custom note", "This is a second custom note"]
    }
) in
43 | Is42
