### /file.ncl
{
  Schema = {
    foo | { bar },
    baz,
  },
  some_func
    | Schema -> Dyn
    = fun src =>
      {
        some_val = src.foo,
      },
}
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///file.ncl"
### position = { line = 9, character = 23 }
### context = { triggerKind = 2, triggerCharacter = "." }
