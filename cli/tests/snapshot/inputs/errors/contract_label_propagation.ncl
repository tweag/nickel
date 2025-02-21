# capture = 'stderr'
# command = ['eval']
let Contract =
  std.contract.custom (fun lbl value =>
    value
    |> std.array.fold_left
      (fun acc x => std.contract.check Number lbl x |> match { 'Ok _v => acc, 'Error e => 'Error e })
      'Ok
  )
in

[1, 2, "hi"] | Contract
