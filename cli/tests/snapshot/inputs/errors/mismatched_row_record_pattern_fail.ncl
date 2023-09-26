# capture = 'stderr'
# command = ['eval']
(
  let { x : { a : Number } = { a : String } } = { x = { a = true } } 
  in a
) : _
