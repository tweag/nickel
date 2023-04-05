{
  compat
    | doc m%"
        Nix compatibility layer.

        This library is used by program transpiled from Nix code to Nickel. This
        library should'nt usually be used directly by Nickel program. The API
        isn't stable and no backward-compatibility guarantees exist at this
        point
      "%
  = {
    # Addition in Nix is overloaded to work both as number addition and string
    # concatenation. There is no such operator in Nickel. This function
    # implement the equivalent of the Nix primitive operator by dynamically
    # dispatching between addition and concatenation, based on the runtime type
    # of its arguments.
    add = fun a b =>
      if %typeof% a == `Str && %typeof% b == `Str
      then
        a ++ b
      else
        a + b,

    # The update operator of Nix `//`. It's a "general form" of the
    # `record.update` of Nickel.
    #
    # TODO: May be interesting to be adapted and integrated to the actual Nickel
    # stdlib.
    update_all = fun r1 r2 =>
      r2
      |> record.fields
      |> array.fold_left (fun acc key => record.update key r2."%{key}" acc) r1,

    has_field_path = fun fields record =>
       # Because it's only used by generated code, this length will never be
       # initially 0. So if it's 0, it mean the end of the path.
       let head = %head% fields in
       %length% fields == 0 ||
       (
          %has_field% head record &&
          has_field_path (%tail% fields) record."%{head}"
       ),

    with =
      let AssertFound = fun label value =>
        if value.found then
          value.value
        else
          %blame% label
      in

      fun envs field => (
        array.fold_right (fun current acc =>
          if !acc.found && record.has_field field current
          then { value = current."%{field}", found = true}
          else acc
        )   {value = null, found = false} envs
      ) | AssertFound
  }
}
