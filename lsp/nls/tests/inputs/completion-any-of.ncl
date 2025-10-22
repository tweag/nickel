### /file.ncl
let rec
  C = { field_in_c },
  D = { field_in_d },
  both = [C, D],
in
[
  {
    a | std.contract.any_of [C, D] = { field },
    b | std.contract.all_of [C, D] = { field },
  },
  let ctr = std.contract in
  {
    a | ctr.any_of [C, D] = { field },
  },
  let ctr = std.contact in # Typo!
  {
    a | ctr.any_of [C, D] = { field },
  },
  let rec
    { contract, .. } = std,
    blah = contract.any_of,
    foo = std.contract.any_of,
  in
  {
    a | blah [C, D] = { field },
    b | contract.any_of [C, D] = { field },
    c | foo [C, D] = { field },
    d | foo both = { field },
    e | std.contract.Sequence [C, D] = { field },
  },
]
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///file.ncl"
### position = { line = 7, character = 44 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///file.ncl"
### position = { line = 8, character = 44 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///file.ncl"
### position = { line = 12, character = 35 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///file.ncl"
### position = { line = 16, character = 35 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///file.ncl"
### position = { line = 24, character = 29 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///file.ncl"
### position = { line = 25, character = 40 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///file.ncl"
### position = { line = 26, character = 28 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///file.ncl"
### position = { line = 27, character = 26 }
###
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///file.ncl"
### position = { line = 28, character = 46 }
