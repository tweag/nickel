# capture = 'stderr'
# command = []
({ "%{(if false then "foo" else "bar")}" = false, bar = true }).foo