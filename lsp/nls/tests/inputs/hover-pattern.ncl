### /main.ncl
let outer@{ foo = inner@{ bar = innermost } } = {
  foo | doc "middle" = {
    bar | Number | doc "innermost" = 3
  }
}
in
[
  outer,
  outer.foo,
  inner.bar,
  innermost,
]
### [[request]] # outer
### type = "Hover"
### textDocument.uri = "file:///main.ncl"
### position = { line = 7, character = 3 }
###
### [[request]] # the next outer
### type = "Hover"
### textDocument.uri = "file:///main.ncl"
### position = { line = 8, character = 3 }
###
### [[request]] # the foo in outer.foo
### type = "Hover"
### textDocument.uri = "file:///main.ncl"
### position = { line = 8, character = 9 }
###
### [[request]] # inner
### type = "Hover"
### textDocument.uri = "file:///main.ncl"
### position = { line = 9, character = 3 }
###
### [[request]] # the bar in inner.bar
### type = "Hover"
### textDocument.uri = "file:///main.ncl"
### position = { line = 9, character = 9 }
###
### [[request]] # innermost
### type = "Hover"
### textDocument.uri = "file:///main.ncl"
### position = { line = 10, character = 3 }
