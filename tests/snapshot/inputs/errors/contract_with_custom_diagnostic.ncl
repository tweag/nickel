let Contract = fun label _value =>
  label
  |> contract.label.with_message "main error message"
  |> contract.label.with_notes [
      "This is the first note",
      "This is the second note"
    ]
  |> contract.blame
in

null | Contract
