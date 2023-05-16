# capture = 'stderr'
# command = []
(
  let { x = { a : Number }} = { x = { a = "" }} in
  x
) : _