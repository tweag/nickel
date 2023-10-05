### /goto.ncl
let
  foo = {
    inner = { bar = 1 },
  }
in
let
  { inner } = foo
in
let
  { inner = blah@{ bar } } = foo
in
  [
    inner.bar,
    bar
  ]
### [[request]] # the `inner` in `let { inner } = foo`
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 6, character = 7 }
###
### # the `inner` in `let { inner = blah@{ bar } } = foo`
### # Note that this `inner` is not a binding, so we don't expect
### # it to goto anything.
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 9, character = 7 }
###
### [[request]] # the `blah` in `let { inner = blah@{ bar } } = foo`
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 9, character = 14 }
###
### [[request]] # the `bar` in `let { inner = blah@{ bar } } = foo`
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 9, character = 20 }
###
### [[request]] # the `inner` in `inner.bar`
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 12, character = 7 }
###
### [[request]] # the `bar` in `inner.bar`
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 12, character = 11 }
###
### [[request]] # the `bar` in `bar`
### type = "GotoDefinition"
### textDocument.uri = "file:///goto.ncl"
### position = { line = 13, character = 5 }
