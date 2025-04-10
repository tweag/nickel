### /file.ncl
{
  a = 'Ok { ba },
  b = 'Error { ba },
  c = 'Nested { a = { inn } },
} | { _ : [| 'Ok { bar }, 'Error { baz }, 'Nested { a | { inner } } |] }
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///file.ncl"
### position = { line = 1, character = 14 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///file.ncl"
### position = { line = 2, character = 17 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///file.ncl"
### position = { line = 3, character = 25 }
