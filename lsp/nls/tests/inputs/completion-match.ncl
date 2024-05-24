### /input.ncl
let p = 1 in
{
  foo = { bar = 1 },
  baz,
} |> match {
 'Bar payload => p,
 x => x.foo.ba,
 { foo = x } if x.ba == 1 => 3,
 # In this next one, the match isn't applied so there will be no completions for the `blah`,
 # but there should be a completion for `p`.
 y => match { payload => p.blah },
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
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///input.ncl"
### position = { line = 10, character = 26 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///input.ncl"
### position = { line = 10, character = 29 }
