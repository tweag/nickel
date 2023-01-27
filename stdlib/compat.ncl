{
compat | doc "Nix compatibility layer. This library should not be used by Nickel program."
= {
  # Addition in Nix is overloaded to work both as number addition and string concatenation.
  # There is no such operator in Nickel. This function implement the equivalent of the Nix primitive operator by dynamically
  # dispatching between addition and concatenation, based on the runtime type of its arguments.
  add = fun a b =>
    if %typeof% a == `Str && %typeof% b == `Str
    then a ++ b
    else a + b,

  # The update operator of Nix `//`. It's a "general form" of the `record.update` of Nickel.
  # TODO: May be intresting to be addapted and integrated the the actual Nickel stdlib.
  update_from_rec = fun r1 r2 =>
    array.fold
      (fun key acc => record.update key r2."%{key}" acc)
      r1
      (record.fields r2),

  has_field_path = fun fields record =>
      %length% fields == 0 || # Because it's only used by generated code, this length will never be initialy 0. So if it's 0, it mean the end of the path.
      (
        %has_field% (%head% fields) record &&
	has_field_path (%tail% fields) record."%{%head% fields}"
      ),

  with =
    let AssertFound = fun l val => if val.found 
      then val.value
      else  %blame% l in
    fun envs field => (
    array.fold (fun current acc =>
      if !acc.found && record.has_field field current
      then { value = current."%{field}", found = true}
      else acc
  ) {value = null, found = false} envs) | AssertFound
}
}
