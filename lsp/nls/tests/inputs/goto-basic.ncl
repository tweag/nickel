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
###
### [[request]]
### type = "References"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 1, character = 3 }
### context = { includeDeclaration = true }
###
### [[request]]
### type = "References"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 1, character = 3 }
### context = { includeDeclaration = false }
###
### [[request]]
### type = "References"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 3, character = 3 }
### context = { includeDeclaration = true }
###
### [[request]]
### type = "References"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 3, character = 3 }
### context = { includeDeclaration = false }
###
### [[request]]
### type = "References"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 3, character = 9 }
### context = { includeDeclaration = true }
###
### [[request]]
### type = "References"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 3, character = 9 }
### context = { includeDeclaration = false }
