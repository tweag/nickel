let Assert = fun l x => x || %blame% l in

// piecewise_definitions
({foo.bar.stuff = 1} == {foo = {bar = {stuff = 1}}} | #Assert) &&
({foo.bar.stuff = 1, foo.bar.stuff = 1} == {foo = {bar = {stuff = 1}}}
  | #Assert) &&
({a.b.c.d = "foo"} == {a = {b = {c = {d = "foo"}}}} | #Assert) &&
({a.b = "foo", a.c = "bar"} == {a = {b = "foo", c = "bar"}} | #Assert) &&
({a.b.c = "foo", a.b.d = {foo = 1}, a.b.d = {bar = 2}}
  == {a = {b = {c = "foo", d = {foo = 1, bar = 2}}}}
  | #Assert) &&

({foo.bar.baz = 1, foo.bar.stuff = false}
  == {foo = {bar = {baz = 1, stuff = false}}}
  | #Assert) &&

({foo = {}, foo.bar = {}, foo.bar.baz = true}
  == {foo = {bar = {baz = true}}}
  | #Assert) &&

// quoted_fields
({"foo$-^bar" = "foo"}."foo$-^bar" == "foo" | #Assert) &&
(let x = "fo" ++ "o" in
  {"#{x}-bar" = "foo"}."foo-bar" == "foo" | #Assert) &&

({"foo-bar"."baz+baz" = false}."foo-bar"."baz+baz" == false | #Assert) &&

// recursive_paths
({foo.bar.baz = 1, bar.baz.foo = foo.bar.baz + 1}
  == {foo = {bar = {baz = 1}}, bar = {baz = {foo = 2}}}
  | #Assert) &&
({foo.bar.baz = 1, bar.baz.foo = foo.bar.baz + 1}
  == {foo = {bar = {baz = 1}}, bar = {baz = {foo = 2}}}
  | #Assert) &&


// piecewise_annotations
({foo.bar | default = 1, foo.baz = 2}
  == {foo = {bar = 1, baz = 2}}
  | #Assert) &&
({foo.bar | default = 1, foo.bar = 2} == {foo = {bar = 2}} | #Assert) &&
({foo.bar.baz | Bool = true}.foo.bar.baz | #Assert) &&

// recursive_dynamic_fields
(let x = "foo" in
  {"#{x}" = bar, bar = 1} == {foo = 1, bar = 1} | #Assert) &&
({"#foo"."#bar".baz = other + 1, other = 0}."#foo"."#bar".baz == 1) &&


true
