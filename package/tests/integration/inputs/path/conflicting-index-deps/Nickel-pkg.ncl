{
  name = "conflicting-index-deps",
  description = "A package with conflicting index dependencies",
  version = "0.1.0",
  authors = ["Joe"],
  minimal_nickel_version = "1.9.0",
  dependencies = {
    dep = 'Path "../single-index-dep", # this depends on leaf =0.1.0
    leaf = 'Index { package = "github:example/leaf", version = "=0.1.1" }
  },
} | std.package.Manifest
