### /file.ncl
let Foo = { foo | String } in
let Bar = { bar | Number } in

let f | std.contract.any_of [Foo, Bar] -> Dyn = fun x => x.ba in
let contracts = [Foo, Bar] in
let f | std.contract.any_of contracts -> Dyn = fun x => x.fo in
let just_foo = [Foo] in
let f | std.contract.any_of just_foo -> Dyn = fun x => x.fo in
0
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///file.ncl"
### position = { line = 3, character = 59 }
### context = { triggerKind = 2, triggerCharacter = "." }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///file.ncl"
### position = { line = 5, character = 58 }
### context = { triggerKind = 2, triggerCharacter = "." }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///file.ncl"
### position = { line = 7, character = 57 }
### context = { triggerKind = 2, triggerCharacter = "." }
