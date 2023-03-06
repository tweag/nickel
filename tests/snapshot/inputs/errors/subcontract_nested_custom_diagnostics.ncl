let ChildContract = fun label value =>
  label
  |> contract.label.with_message "child's message"
  |> contract.label.append_note "child's note"
  |> contract.blame
in

let ParentContract = fun label value =>
  let label =
    label
    |> contract.label.with_message "parent's message"
    |> contract.label.append_note "parent's note"
  in
  contract.apply ChildContract label value
in

null | ParentContract
