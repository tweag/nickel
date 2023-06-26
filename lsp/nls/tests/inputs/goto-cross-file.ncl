### /dep.ncl
{ foo = "val" }
### /goto.ncl
let
  record = import "dep.ncl"
in
  record.foo
### GotoDefinition /goto.ncl:3:2
### GotoDefinition /goto.ncl:3:9
