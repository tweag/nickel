# test.type = 'pass'
let test1 : {foo : Number} = {foo = 5} in
let _ = (std.record.insert "bar" 5 test1) : {_ : Number} in
let test2 : {foo : Number, bar : Number} = {foo = 5, bar = 5} in
let _ = (std.record.insert "baz" 5 test2) : {_ : Number} in
true
