let f = fun y => merge ((fun x => {a=y;}) 0) ({b=false;}) in
(f 1).a
