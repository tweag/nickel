[
  (import leaf010).version == "0.1.0",
  (import leaf020).version == "0.2.0",
  (import subdir).version == "0.2.0/subdir",
]
|> std.test.assert_all
