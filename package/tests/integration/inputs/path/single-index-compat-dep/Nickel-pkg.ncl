{
  name = "single-index-compat-dep",
  description = "A package with an index dependency that doesn't have an exact constraint",
  version = "0.1.0",
  authors = ["Joe"],
  minimal_nickel_version = "1.9.0",
  dependencies = {
    leaf = 'Index { package = "github:example/leaf", version = "0.1.0" }
  },
} | std.package.Manifest
