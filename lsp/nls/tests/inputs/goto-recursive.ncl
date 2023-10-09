### /main.ncl
({ a = { b = a } } & { a = a.b.b }).a.b
### # Test the two definition sites of `a`.
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///main.ncl"
### position = { line = 0, character = 13 }
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///main.ncl"
### position = { line = 0, character = 27 }
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///main.ncl"
### position = { line = 0, character = 36 }
###
### # Test the two references of `a`. There are three, actually,
### # but the last one doesn't work yet because we only track
### # Term::Vars as usages.
###
### [[request]]
### type = "References"
### textDocument.uri = "file:///main.ncl"
### position = { line = 0, character = 3 }
### context = { includeDeclaration = false }
###
### [[request]]
### type = "References"
### textDocument.uri = "file:///main.ncl"
### position = { line = 0, character = 13 }
### context = { includeDeclaration = false }
###
### [[request]]
### type = "References"
### textDocument.uri = "file:///main.ncl"
### position = { line = 0, character = 23 }
### context = { includeDeclaration = false }
###
### [[request]]
### type = "References"
### textDocument.uri = "file:///main.ncl"
### position = { line = 0, character = 27 }
### context = { includeDeclaration = false }
###
### [[request]]
### type = "References"
### textDocument.uri = "file:///main.ncl"
### position = { line = 0, character = 36 }
### context = { includeDeclaration = false }
###
### # Test the definition of `b`.
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///main.ncl"
### position = { line = 0, character = 29 }
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///main.ncl"
### position = { line = 0, character = 31 }
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///main.ncl"
### position = { line = 0, character = 38 }
