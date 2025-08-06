### /unparseable.ncl
[1,2 : Array Number
### /diagnostics-unparseable-import-causes-typechecking-error.ncl

# The inferred type of an unparseable file is always Dyn, which means
# x : Array Number will fail to typecheck here. The main purpose of this test
# is to make sure that a diagnostic for the unparseable import is still
# published even if it causes a later typechecking error, otherwise it might
# be difficult to track down the actual problem.
let x = import "unparseable.ncl"
in x : Array Number
### diagnostic = ["file:///diagnostics-unparseable-import-causes-typechecking-error.ncl"]
