let ascending = fun x y =>
  if x < y then
    'Lesser
  else if x == y then
    'Equal
  else
    'Greater
in

{ run = std.array.sort ascending }
