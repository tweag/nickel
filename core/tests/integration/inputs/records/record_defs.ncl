# test.type = 'pass'


[
  # piecewise_definitions
  {foo.bar.stuff = 1} == {foo = {bar = {stuff = 1}}},
  {foo.bar.stuff = 1, foo.bar.stuff = 1} == {foo = {bar = {stuff = 1}}},
  {a.b.c.d = "foo"} == {a = {b = {c = {d = "foo"}}}},
  {a.b = "foo", a.c = "bar"} == {a = {b = "foo", c = "bar"}},
  {a.b.c = "foo", a.b.d = {foo = 1}, a.b.d = {bar = 2}}
    == {a = {b = {c = "foo", d = {foo = 1, bar = 2}}}},

  {foo.bar.baz = 1, foo.bar.stuff = false}
    == {foo = {bar = {baz = 1, stuff = false}}},

  {foo = {}, foo.bar = {}, foo.bar.baz = true}
    == {foo = {bar = {baz = true}}},

  # quoted_fields
  {"foo$-^bar" = "foo"}."foo$-^bar" == "foo",
  let x = "fo" ++ "o" in
    {"%{x}-bar" = "foo"}."foo-bar" == "foo",

  {"foo-bar"."baz+baz" = false}."foo-bar"."baz+baz" == false,
  {foo-bar.baz-baz = false}.foo-bar.baz-baz == false,

  # recursive_paths
  {foo.bar.baz = 1, bar.baz.foo = foo.bar.baz + 1}
    == {foo = {bar = {baz = 1}}, bar = {baz = {foo = 2}}},
  {foo."bar"."baz" = 1, "bar"."baz"."foo" = foo.bar."baz" + 1}
    == {foo = {bar = {baz = 1}}, bar = {baz = {foo = 2}}},


  # piecewise_annotations
  {foo.bar | default = 1, foo.baz = 2}
    == {foo = {bar = 1, baz = 2}},
  {foo.bar | default = 1, foo.bar = 2} == {foo = {bar = 2}},
  {foo.bar.baz | Bool = true}.foo.bar.baz,

  # recursive_dynamic_fields
  let x = "foo" in {"%{x}" = bar, bar = 1} == {foo = 1, bar = 1},
  ({"%foo"."%bar".baz = other + 1, other = 0}."%foo"."%bar".baz == 1),
]
|> std.test.assert_all
