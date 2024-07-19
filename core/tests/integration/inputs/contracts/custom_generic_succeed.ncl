# test.type = 'pass'
let AlwaysSucceed = std.contract.custom (fun _ value => 'Ok value) in

(
  3
    | AlwaysSucceed
) == 3
