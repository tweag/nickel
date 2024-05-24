# test.type = 'pass'


[
  # check that a record type literal is indeed converted to the corresponding
  # contract, which shouldn't be a record literal
# std.typeof {foo : String, bar : Number} != `Record,

  # check_types_dont_propagate

  # TODO: restore the test below. The PR which added it is not at fault: the
  # test is failing on master. The issue is that contracts derived from record
  # type seem to erase metadata, while they shouldn't.
  #({foo | default = 5} | {foo : Number}) & {foo = "a"} == {foo = "a"},

  let swap
    : forall a b. {foo : a, bar : b} -> {foo : b, bar : a }
    = fun {foo=prev_foo, bar=prev_bar} => {bar = prev_foo, foo = prev_bar}
  in
  ((swap {foo = 1, bar = "a"})
  & {foo | force = false, bar | force = true})
  == {foo = false, bar = true},
]
|> std.test.assert_all
