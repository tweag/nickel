let {Assert, ..} = import "testlib.ncl" in

let this-isn't-invalid = true in this-isn't-invalid | Assert
