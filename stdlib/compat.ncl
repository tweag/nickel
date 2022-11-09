{
compat | doc "Nix compatibility layer. This library should not be used by Nickel program."
= {

  # The addition in Nix can be a string concatenation.
  # this function handle this behaviour.
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
