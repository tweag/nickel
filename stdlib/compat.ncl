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
