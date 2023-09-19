### /goto.ncl
let person | { name: String } = null in 
# The "person" on the rhs here refers to the person in the line above
let person = person in
person
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 2, character = 16 }
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 3, character = 3 }
###
### [[request]]
### type = "References"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 2, character = 16 }
### context = { includeDeclaration = true }
###
### [[request]]
### type = "References"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 2, character = 16 }
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
