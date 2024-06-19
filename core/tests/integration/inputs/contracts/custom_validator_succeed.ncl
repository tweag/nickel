# test.type = 'pass'
let AlwaysSucceed = std.contract.from_validator (fun _ => 'Ok) in
(3 | AlwaysSucceed) == 3
