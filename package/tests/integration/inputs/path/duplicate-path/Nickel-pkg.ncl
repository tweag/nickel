{
  name = "duplicate-path",
  description = "A package having two different routes in the dependency graph to the same path package",
  version = "0.1.0",
  authors = ["Joe"],
  minimal_nickel_version = "1.9.0",
  dependencies = {
    dep = 'Path "../single-path-dep",
    leafy = 'Path "../leaf",
  },
} | std.package.Manifest
