# capture = 'stderr'
# command = ['eval']
(
  let { x = { a : Number }} = { x = { a = "" }} in
  x
) : _
