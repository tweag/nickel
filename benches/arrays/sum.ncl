let sum
  | Array Num -> Num
  = let rec aux = fun step xs =>
      if step == array.length xs then
        0
      else
        array.elem_at step xs + aux (step + 1) xs
    in
      aux 0
in {
  run = fun n => array.generate (fun x => x + 1) n |> sum
}
