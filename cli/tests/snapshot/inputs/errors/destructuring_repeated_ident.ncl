# capture = 'stderr'
# command = ['eval']
let f = fun { duped, duped, .. } => duped
in f { duped = 1, other = "x" }
