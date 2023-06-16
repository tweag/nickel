# test.type = 'pass'
(std.serialize 'Json (import "imported/serialize.ncl") == (std.serialize 'Json ({ foo = "ab" })))