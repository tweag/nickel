let Assert = fun l x => x || %blame% l in

(import "imported.ncl" 3 == 3 | #Assert) &&

true
