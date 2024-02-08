# test.type = 'error'
#
# [test.metadata]
# error = 'ParseError::DuplicateIdentInRecordPattern'
#
# [test.metadata.expectation]
# ident = 'duped'
let f = fun { duped, duped, .. } => duped
in f { duped = 1, other = "x" }