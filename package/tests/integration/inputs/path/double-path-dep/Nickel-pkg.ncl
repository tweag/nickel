{
  name = "double-path-dep",
  description = "A package with a single path dependency, which has a further path dependency",
  version = "0.1.0",
  authors = ["Joe"],
  minimal_nickel_version = "1.9.0",
  dependencies = {
    dep = 'Path "../single-path-dep"
  },
} | std.package.Manifest
