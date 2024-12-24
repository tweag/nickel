# test.type = 'pass'
[
  ((std.record.insert "baz" 1 { foo = bar + 1, bar = 0 }) & { bar | force = 2 }) == { foo = 1, bar = 2, baz = 1 },
  ((std.record.insert_with_opts "baz" 1 { foo = bar + 1, bar = 0 }) & { bar | force = 2 }) == { foo = 1, bar = 2, baz = 1 },
  (
    (std.record.remove "to_remove" { foo = bar + 1, bar = 0, to_remove = false, rev_dep = to_remove })
    & { bar | force = 1, to_remove = true }
  ) == { foo = 1, bar = 1, to_remove = true, rev_dep = false },
  (
    (std.record.remove_with_opts "to_remove" { foo = bar + 1, bar = 0, to_remove = false, rev_dep = to_remove })
    & { bar | force = 1, to_remove = true }
  ) == { foo = 1, bar = 1, to_remove = true, rev_dep = false },
  ((std.record.update "bar" 1 { foo = bar + 1, bar = 0 }) & { bar | force = 2 }) == { foo = 1, bar = 2, },
]
|> std.test.assert_all
