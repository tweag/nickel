# test.type = 'pass'


# tests related to RFC005. Check in particular that the motivating examples of
# RFC005 are indeed fixed by lazy propagation.
[
  # first example from RFC005
  ({
    foo | {
      bar : Number,
      baz : String
    }
  }
  & {foo = {}}
  & {foo.bar = 1}
  & {foo.baz = "a"})
  == { foo.bar = 1, foo.baz = "a" },

  # Nixel-inspired example from RFC005
  let Drv = { out_path | String, ..} in
  let Package = { name | String, drv | Drv, .. } in
  ({
    build_inputs | {_: Package} = {
      foo,
      bar,
    },
    build = m%"
      %{build_inputs.foo.drv.out_path}/bin/foo $out
    "%,
  } & {
    build_inputs = {
      foo = { name = "foo", drv.out_path = "/fake/path" },
      bar = { name = "bar", drv.out_path = "/fake/path" },
    }
  })
  == {
    build_inputs.foo = { name = "foo", drv.out_path = "/fake/path" },
    build_inputs.bar = { name = "bar", drv.out_path = "/fake/path" },
    build = "/fake/path/bin/foo $out",
  },

  # "Outer-cross application doesn't make sense" example
  (({foo = 5} | {foo | Number})
  & {bar = "bar"})
  == {foo = 5, bar = "bar"},
]
|> std.test.assert_all
