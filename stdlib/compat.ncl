{
compat | doc "Nix compatibility layer. should not be used by Nickel program."
= {
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
