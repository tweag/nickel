{
  name = "different-versions-index-dep",
  description = "A package with two index dependencies in different versions",
  version = "0.1.0",
  authors = ["Joe"],
  minimal_nickel_version = "1.9.0",
  dependencies = {
    leaf = 'Index { package = "github:example/leaf", version = "=0.1.0" },
    leaf2 = 'Index { package = "github:example/leaf", version = "=0.2.0" },
  },
} | std.package.Manifest
