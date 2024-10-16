### /main.ncl
let baz = { bar = 1 } in
{
  field.foo = baz.
} | { field = { foo } }
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///main.ncl"
### position = { line = 2, character = 18 }
### context = { triggerKind = 2, triggerCharacter = "." }
