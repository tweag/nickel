# capture = 'stderr'
# command = ['eval']
let { x ? true } = { x | default = false } in x
