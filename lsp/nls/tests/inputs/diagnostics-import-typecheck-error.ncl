### /bad_type.ncl
1 : String
### /diagnostics-import-typecheck-error.ncl
let x = import "bad_type.ncl"
in x
### diagnostic = ["file:///diagnostics-import-typecheck-error.ncl"]
