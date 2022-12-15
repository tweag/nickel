let {Assert, ..} = import "lib/assert.ncl" in

let this-isn't-invalid = true in this-isn't-invalid | Assert
