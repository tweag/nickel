# capture = 'stderr'
# command = []
let f = fun { duped, duped, .. } => duped
in f { duped = 1, other = "x" }
