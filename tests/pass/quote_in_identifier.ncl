let Assert = fun l x => x || %blame% l in

let this-isn't-invalid = true in this-isn't-invalid | Assert
