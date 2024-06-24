# test.type = 'pass'
let AlwaysSucceed = std.contract.from_predicate (fun _ => true) in
(3 | AlwaysSucceed) == 3
