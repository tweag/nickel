### /goto.ncl
let record = { foo = 1, bar = { baz = "hi" } } in
let another = record.bar in
[
  record.foo,
  record.bar,
  record.bar.baz,
  another.baz,
]
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 3, character = 9 }
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 4, character = 9 }
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 5, character = 13 }
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 6, character = 10 }
