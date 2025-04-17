# capture = 'stderr'
# command = ['eval']

let foo = {bar.baz = 1} in
{include foo, foo.qux = 2}
