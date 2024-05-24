### /input.ncl
let p = 1 in
{
  foo = { bar = 1 },
  baz,
} |> match {
 'Bar payload => p,
 x => x.foo.ba,
 { foo = x } if x.ba == 1 => 3,
}
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///input.ncl"
### position = { line = 5, character = 18 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///input.ncl"
### position = { line = 6, character = 14 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///input.ncl"
### position = { line = 7, character = 20 }
