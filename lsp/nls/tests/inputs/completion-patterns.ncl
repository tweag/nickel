### /input.ncl
let outer@{ foo = inner@{ bar = innermost, .. }, .. } = {
  extra = 1,
  foo = {
    more = 1,
    bar = {
      most = 1,
      baz = 1,
    }
  },
}
in
[
  outer.foo,
  outer.foo.bar,
  outer.foo.bar.baz,
  inner.bar,
  inner.bar.baz,
  innermost.baz,
]
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///input.ncl"
### position = { line = 12, character = 10 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///input.ncl"
### position = { line = 13, character = 14 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///input.ncl"
### position = { line = 14, character = 18 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///input.ncl"
### position = { line = 15, character = 10 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///input.ncl"
### position = { line = 16, character = 14 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///input.ncl"
### position = { line = 17, character = 14 }
###
### # Env completions
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///input.ncl"
### position = { line = 1, character = 3 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///input.ncl"
### position = { line = 3, character = 5 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///input.ncl"
### position = { line = 5, character = 6 }
