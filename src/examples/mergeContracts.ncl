let Y = fun f => (fun x => f (x x)) (fun x => f (x x)) in
let dec = fun x => x + (-1) in
let or = fun x => fun y => if x then x else y in
let isEven_ = Y (fun f =>
    (fun x =>
        if (isZero x) then true
        else (
            if (isZero (dec x)) then false
            else (f (dec (dec x)))
        )
    )
) in
let isDivBy3_ = Y (fun f =>
    (fun x =>
        if (isZero x) then true
        else (
            if or (isZero (dec (dec x))) (isZero (dec x)) then false
            else (f (dec (dec (dec x))))
        )
    )
) in
let toCtr = fun f => fun l => fun x => (
  if (isNum x) then (
    if (f x) then x else blame l)
  else blame l
) in
let isEven = toCtr isEven_ in
let isDivBy3 = toCtr isDivBy3_ in
let composed = merge Contract(#isEven) Contract(#isDivBy3) in
merge 12 composed
