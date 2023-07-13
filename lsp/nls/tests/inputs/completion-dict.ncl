### /input-inline.ncl
let x | { _ : { foo : Number | default = 1 } } = {} in
x.PATH.
### /input-assign.ncl
let Dict = { _ : { foo : Number | default = 1 } } in
let x | Dict = {} in
x.PATH.
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///input-inline.ncl"
### position = { line = 1, character = 7 }
### context = { triggerKind = 2, triggerCharacter = "." }
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///input-assign.ncl"
### position = { line = 2, character = 7 }
### context = { triggerKind = 2, triggerCharacter = "." }
