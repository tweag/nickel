### /included.ncl
{
  lalala = 1,
  "has a space" = { falala = 2 },
}
### /completion-basic.ncl
let config = {
  version = "1.2",
  verified = { really = true },
  foo = "bar",
}
in
[
  {
    a = config.version,
    b = config.verified.really,
  },
  (config & config.verified).real,
  (import "included.ncl").lala
  (import "included.ncl")."has a space".fala
  ({} | { field = 1 }).fiel
  ({} | (let x = { field = 1 } in x)).fiel
  ({} | { field = { subfield = 1 } }.field).subfiel
]
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///completion-basic.ncl"
### position = { line = 8, character = 12 }
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
### position = { line = 9, character = 15 }
### context = { triggerKind = 2, triggerCharacter = "." }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///completion-basic.ncl"
### position = { line = 9, character = 23 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///completion-basic.ncl"
### position = { line = 9, character = 24 }
### context = { triggerKind = 2, triggerCharacter = "." }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///completion-basic.ncl"
### position = { line = 9, character = 27 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///completion-basic.ncl"
### position = { line = 11, character = 31 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///completion-basic.ncl"
### position = { line = 12, character = 30 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///completion-basic.ncl"
### position = { line = 13, character = 44 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///completion-basic.ncl"
### position = { line = 14, character = 27 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///completion-basic.ncl"
### position = { line = 15, character = 42 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///completion-basic.ncl"
### position = { line = 16, character = 51 }
