# capture = 'stderr'
# command = ['eval']
let ChildContract = fun label value =>
  label
  |> std.contract.label.with_message "child's message"
  |> std.contract.label.append_note "child's note"
  |> std.contract.blame
in

let ParentContract = fun label value =>
  let label =
    label
    |> std.contract.label.with_message "parent's message"
    |> std.contract.label.append_note "parent's note"
  in
  std.contract.apply ChildContract label value
in

null | ParentContract
