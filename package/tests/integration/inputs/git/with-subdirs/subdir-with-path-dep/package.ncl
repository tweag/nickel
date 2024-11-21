{
  name = "subdir-with-path-dep",
  description = "A package in a subdir of a git repo, depending on packages in other directories",
  version = "0.1.0",
  authors = ["Joe"],
  nickel_version = "1.9.0",
  dependencies = {
    leaf = 'Path "../leaf-subdir",
    parent = 'Path "..",
  },
} | std.package.Manifest
