{
  name = "path-to-index-dep",
  description = "A package with a path dependency that has an index dependency",
  version = "0.1.0",
  authors = ["Joe"],
  minimal_nickel_version = "1.9.0",
  dependencies = {
    dep = 'Path "../single-index-dep",
  },
} | std.package.Manifest
