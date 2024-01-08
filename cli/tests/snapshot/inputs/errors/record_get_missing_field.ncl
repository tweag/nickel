# capture = 'stderr'
# command = ['eval']
let record = {
  foo = 1,
  bar = "two",
  baz = false,
}
in

record
|> std.record.remove "foo"
|> std.record.get "foo"
