### /syms.ncl
let foo = 3 in
let func = fun x => 1 in
{
  name = "value",
  other_name = {
    inner_name = let inner_binding = 3 in "value",
  },
  type_checked_block = {
    inner_name = { name = "value" },
  } : _,
  annotated | String,
  docced | doc "The symbol range doesn't currently include docs",
}
### [[request]]
### type = "Symbols"
### textDocument.uri = "file:///syms.ncl"
