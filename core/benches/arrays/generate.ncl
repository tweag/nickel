let g = fun n => n * 2 + 5 in
{
  unchecked = {
    generate = fun n g =>
      if n == 0 then
        []
      else
        generate (n - 1) g @ [g n],

    run = fun n => generate n g,
  },
  checked = {
    generate_with_contract | forall a. Number -> (Number -> a) -> Array a
      = fun n g =>
        if n == 0 then
          []
        else
          generate_with_contract (n - 1) g @ [g n],

    run = fun n =>
      generate_with_contract n g,
  },
}
