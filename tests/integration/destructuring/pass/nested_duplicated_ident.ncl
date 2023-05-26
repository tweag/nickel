# test.type = 'pass'
let f = fun { x = { y }, z = { y } } => y in
let result = f { x = { y = 1 }, z = { y = 2 } } in
result == 1