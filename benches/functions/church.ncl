{
  encode = fun n f x => if n == 0 then x else encode (n - 1) f (f x),
  decode = fun nApply => nApply (fun x => x + 1) 0,

  add = fun n m f x => n f (m f x),
  mult = fun n m f x => n (m f) x,

  generate = fun f n =>
    if n == 0 then
      []
    else
      (generate f (n - 1)) @ [f n],

  id = fun x => x,

  run = fun n =>
    let orig = generate id n in
    let encoded = lists.map encode orig in

    let listChurch = lists.map (fun nChurch =>
      add (mult (encode 3) nChurch) (encode 5))
      encoded in
    let sumChurch = lists.fold (fun accChurch nChurch => add accChurch nChurch)
      (encode 0) listChurch in

    let list = lists.map (fun n => 3*n + 5) orig in
    let sum = lists.foldl (fun acc n => acc + n) 0 list in
    decode sumChurch == sum,
}
