### /main.ncl
let C = { foo | Number | doc "Some doc" } in
{
  foo | String,
  bar = {
    f
  } | C
}
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///main.ncl"
### position = { line = 4, character = 5 }
