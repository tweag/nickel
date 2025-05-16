# capture = 'stdout'
# command = [ 'query', '--field', 'x.y', '--format', 'json' ]
let y = 1 in
let x = { include y | doc "Doc of y" | default } in
{ include x }
