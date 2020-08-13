let r1 = {
    a = false;
    b = if true then 1 + 1 else 2 + 0;
    c = ((fun x => x) (fun y => y)) 2;
} in
let r2 = {
    b = ((fun x => x) (fun y => y)) 2;
    c = if true then 1 + 1 else 2 + 0;
    d = true;
} in
(merge r1 r2).b
