### /main.ncl
{
  foo = { bar = 2 },
  blah = foo.bar,
  baz.path.path = 2,
}
| {
  foo | doc "outer" = {
    bar | Number | doc "inner" | default = 3
  },
  baz.path | doc "longer path"
}
### [[request]]
### type = "Hover"
### textDocument.uri = "file:///main.ncl"
### position = { line = 1, character = 3 }
###
### [[request]]
### type = "Hover"
### textDocument.uri = "file:///main.ncl"
### position = { line = 1, character = 11 }
###
### [[request]]
### type = "Hover"
### textDocument.uri = "file:///main.ncl"
### position = { line = 2, character = 10 }
###
### [[request]]
### type = "Hover"
### textDocument.uri = "file:///main.ncl"
### position = { line = 2, character = 14 }
###
### [[request]]
### type = "Hover"
### textDocument.uri = "file:///main.ncl"
### position = { line = 3, character = 6 }
