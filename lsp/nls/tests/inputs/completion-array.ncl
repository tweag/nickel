### /file.ncl
{
  a = [ { f } ] | Array { foo },
  b = { c = [ { b } ] },
} | { b | { c | Array { bar } }, .. }
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///file.ncl"
### position = { line = 1, character = 11 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///file.ncl"
### position = { line = 2, character = 17 }
