let {Assert, ..} = import "lib/assert.ncl" in

(import "lib/imported.ncl" 3 == 3 | Assert)
