{
  encode = fun n f x => if n == 0 then x else encode (n-1) f (f x),
  decode = fun nApply => nApply (fun x => x + 1) 0,

  add = fun n m f x => n f (m f x),
  mult = fun n m f x => n (m f) x,

  generate = fun f n =>
    if n == 0 then
      []
    else
      (generate f (n-1)) @ [f n],

  id = fun x => x,

  run = fun n =>
    let orig = generate id n in
    let encoded = lists.map encode orig in

    let lChurch = lists.map (fun nChurch => mult nChurch nChurch) encoded in
    let lChurch = lists.map (fun nChurch => add nChurch nChurch) lChurch in
    let rChurch = lists.fold (fun nChurch acc => mult nChurch acc) lChurch (encode 1) in

    let l = lists.map (fun n => 2*n*n) orig in
    let r = lists.foldl (fun acc n => acc*n) 1 l in
    decode rChurch == r,
}
