# test.type = 'pass'
let AlwaysSucceedImmediate = std.contract.custom (fun _ => 'Done) (fun label _ => std.contract.blame label) in
let AlwaysSucceedFull = std.contract.custom (fun _ => 'Ok) (fun _ value => value) in

(
  3
    | AlwaysSucceedImmediate
    | AlwaysSucceedFull
) == 3
