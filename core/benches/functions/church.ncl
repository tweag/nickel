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
    let base = generate id n in

    let encoded =
      base
      |> std.array.map encode
      |> std.array.map
        (
          fun nChurch =>
            add (mult (encode 3) nChurch) (encode 5)
        )
      |> std.array.fold_right add (encode 0)
    in

    let decoded =
      base
      |> std.array.map (fun n => 3 * n + 5)
      |> std.array.fold_left (fun acc n => acc + n) 0
    in

    decode encoded == decoded,
}
