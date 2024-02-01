### /base.ncl
let foo = import "dep.ncl" in
{
    x = foo.included,
    y = { foo = x },
}
### /dep.ncl
{
  included = 5
}
### # Rename foo to a bar (but not the "foo" in "foo = x")
### [[request]]
### type = "Rename"
### textDocument.uri = "file:///base.ncl"
### position = { line = 0, character = 5 }
### newName = "bar"
###
### [[request]]
### type = "Rename"
### textDocument.uri = "file:///base.ncl"
### position = { line = 2, character = 9 }
### newName = "bar"
###
### # Rename "foo" in "foo = x" (shouldn't rename the others)
### [[request]]
### type = "Rename"
### textDocument.uri = "file:///base.ncl"
### position = { line = 3, character = 11 }
### newName = "baz"
###
### # Rename x to y
### [[request]]
### type = "Rename"
### textDocument.uri = "file:///base.ncl"
### position = { line = 2, character = 4 }
### newName = "y"
###
### [[request]]
### type = "Rename"
### textDocument.uri = "file:///base.ncl"
### position = { line = 3, character = 16 }
### newName = "y"
###
### # Cross-file rename
### [[request]]
### type = "Rename"
### textDocument.uri = "file:///base.ncl"
### position = { line = 2, character = 15 }
### newName = "dependency"
###
### [[request]]
### type = "Rename"
### textDocument.uri = "file:///dep.ncl"
### position = { line = 1, character = 5 }
### newName = "dependency"
