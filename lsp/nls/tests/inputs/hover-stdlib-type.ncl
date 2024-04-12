### /main.ncl
let foo = { bar.baz : String -> Number = fun x => 1 }
in
foo.bar.baz "0" + std.string.to_number "1"
### [[request]]
### type = "Hover"
### textDocument.uri = "file:///main.ncl"
### position = { line = 2, character = 10 }
### [[request]]
### type = "Hover"
### textDocument.uri = "file:///main.ncl"
### position = { line = 2, character = 30 }
