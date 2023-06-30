### /completion-basic.ncl
let config = {
  version = "1.2",
  verified = { really = true },
  foo = "bar",
}
in
{
    a = config.version,
    b = config.verified.really,
}
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///completion-basic.ncl"
### position = { line = 7, character = 12 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///completion-basic.ncl"
### position = { line = 7, character = 15 }
### context = { triggerKind = 2, triggerCharacter = "." }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///completion-basic.ncl"
### position = { line = 8, character = 15 }
### context = { triggerKind = 2, triggerCharacter = "." }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///completion-basic.ncl"
### position = { line = 8, character = 23 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///completion-basic.ncl"
### position = { line = 8, character = 24 }
### context = { triggerKind = 2, triggerCharacter = "." }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///completion-basic.ncl"
### position = { line = 8, character = 27 }
