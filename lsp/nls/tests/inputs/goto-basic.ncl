### /goto.ncl
let
  var = "val"
in
{ foo = var }
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 3, character = 8 }
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 3, character = 9 }
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 3, character = 10 }
