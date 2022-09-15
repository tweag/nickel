let {Assert, ..} = import "testlib.ncl" in

(import "imported.ncl" 3 == 3 | Assert)
