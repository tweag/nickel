# test.type = 'pass'


[
  (import "imported/empty.yaml") == null,

  (import "imported/multiple.yaml") == [
    { type = "event", id = 1 },
    { type = "event", id = 2 }
  ],
]
|> std.test.assert_all
