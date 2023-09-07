let map_cartesian
  : forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c
  = fun f left right =>
    std.array.fold_left
      (
        fun acc_ext left =>
          acc_ext
          @ (
            std.array.fold_left (fun acc_int right => std.array.append (f left right) acc_int) [] right
          )
      )
      []
      left
  in
map_cartesian (++) ["1", "2", "3"] ["a", "b", "c"]
