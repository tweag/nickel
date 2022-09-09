{
compat | doc "Nix compatibility layer. should not be used by Nickel program."
= {
  with: Array {_: Dyn} -> Str -> Dyn
  = fun envs field => (
    array.fold (fun prev current =>
      if record.has_field field current
      then current
      else prev
  ) {} envs)."%{field}"
}
}
