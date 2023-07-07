### /base.ncl
{
  foo = 1
}
### /dep.ncl
let x = (import "base.ncl") in [ x.bar, x.foo ]
### /base.ncl
{
  bar = 1
}
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///dep.ncl"
### position = { line = 0, character = 35 } 
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///dep.ncl"
### position = { line = 0, character = 42 } 
###
### # When base.ncl gets reloaded, dep.ncl should get re-checked. Thus,
### # the reference to bar should get resolved while the reference to foo
### # should not.
