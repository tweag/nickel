# capture = 'stderr'
# command = ['eval']
let Contract = fun label _value =>
  label
  |> std.contract.label.with_message "main error message"
  |> std.contract.label.with_notes [
      "This is the first note",
      "This is the second note"
    ]
  |> std.contract.blame
in

null | Contract
