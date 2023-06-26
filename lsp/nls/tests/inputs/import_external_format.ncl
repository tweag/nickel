### /external_import.ncl
let foo = import "external_import.json" in
let bar = import "external_import.yaml" in
foo & bar
# This request isn't really useful, this test simply checks that the LSP can
# linearize this file without crashing. However, the LSP test suite currently
# requires that we have at least one request, so here it is.
### GotoDefinition /external_import.ncl:3:1
