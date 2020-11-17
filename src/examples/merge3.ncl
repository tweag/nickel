let rNest1 = {b={c=10}} in
let rNest2 = (fun x => {a=x; b={c=x}}) 10 in
let r = rNest1 & rNest2 in
r.b.c
