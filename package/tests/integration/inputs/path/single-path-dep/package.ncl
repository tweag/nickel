{
  name = "single-path-dep",
  description = "A package with a single path dependency",
  version = "0.1.0",
  authors = ["Joe"],
  nickel_version = "1.9.0",
  dependencies = {
    leaf = 'Path "../leaf"
  },
} | std.package.Manifest
