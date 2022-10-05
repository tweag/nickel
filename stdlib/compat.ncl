{
compat | doc "Nix compatibility layer. should not be used by Nickel program."
= {
  with: Array {_: Dyn} -> Str -> Dyn
  = fun envs field => (
    array.fold (fun current prev =>
      if record.has_field field prev
      then prev
      else current
  ) {} envs)."%{field}"
}
}
