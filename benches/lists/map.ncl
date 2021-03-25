let f = { f = fun x => if (x == 0) then 0 else f (x - 1) }.f in
{
  run = fun n =>
    lists.map f (lists.generate builtins.id n)
}
