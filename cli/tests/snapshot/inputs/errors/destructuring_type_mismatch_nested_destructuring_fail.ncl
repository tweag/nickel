# capture = 'stderr'
# command = ['eval']
(let { a : { b : Number } = { b }} = { a = { b = "no" }} in
b) : _
