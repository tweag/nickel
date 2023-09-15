### /goto.ncl
let record = { foo = 1, bar = { baz = "hi" } } in
let toMerge = { bar = { baz = "hi", quux = "bye" } } in
let Contract = { baz | String } in
let another | Contract = (record & toMerge).bar in
[
  another.baz,
  another.quux,
  (toMerge.bar | Contract).baz,
]
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 5, character = 12 }
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 6, character = 12 }
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 7, character = 29 }
