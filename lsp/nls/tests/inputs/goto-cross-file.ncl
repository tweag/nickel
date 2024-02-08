### /dep.ncl
{ foo = "val" }
### /goto.ncl
let
  record = import "dep.ncl"
in
  record.foo
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 1, character = 16 }
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 3, character = 2 }
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 3, character = 9 }
###
### [[request]]
### type = "References"
### textDocument.uri = "file:///dep.ncl"
### position = { line = 0, character = 3 }
### context = { includeDeclaration = false }
