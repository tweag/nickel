{
  name = "single-index-dep",
  description = "A package with an index dependency",
  version = "0.1.0",
  authors = ["Joe"],
  minimal_nickel_version = "1.9.0",
  dependencies = {
    leaf = 'Index { package = "github:example/leaf", version = "=0.1.0" }
  },
} | std.package.Manifest
