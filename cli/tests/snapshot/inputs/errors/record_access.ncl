# capture = 'stderr'
# command = ['eval']
({ "%{(if false then "foo" else "bar")}" = false, bar = true }).foo
