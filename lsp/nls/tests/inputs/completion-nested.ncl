### /completion.ncl
{
  foo = {
    on = 2,
  },
  bar.tw = 1,
  baz.quux.th = 3,
} | {
  foo.one | default = 1,
  bar.two | default = 2,
  baz.quux = {
    three | default = "three",
  }
}
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///completion.ncl"
### position = { line = 2, character = 6 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///completion.ncl"
### position = { line = 4, character = 7 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///completion.ncl"
### position = { line = 5, character = 12 }
