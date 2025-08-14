{
  name = "single-index-dep",
  description = "A package with an index dependency on a package that's in a subdir of a repo",
  version = "0.1.0",
  authors = ["Joe"],
  minimal_nickel_version = "1.9.0",
  dependencies = {
    leaf = 'Index { package = "github:example/leaf/subdir", version = "0.2.0" }
  },
} | std.package.Manifest
