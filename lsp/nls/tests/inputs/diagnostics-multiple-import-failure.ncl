### /bad_type.ncl
2 : String
### /unparseable.ncl
[1,2
### /diagnostics-multiple-import-error.ncl
let x = import "unparseable.ncl" in
let y = import "bad_type.ncl"
in { include [x, y] }
### diagnostic = ["file:///diagnostics-multiple-import-error.ncl"]
