### /goto.ncl
let x = 2 in x +
(let
  y = x,
  x = 1,
in x + y)
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 0, character = 13 }
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 2, character = 6 }
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 4, character = 3 }
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 4, character = 7 }
