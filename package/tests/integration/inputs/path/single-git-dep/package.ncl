{
  name = "single-git-dep",
  description = "A package with a git dependency",
  version = "0.1.0",
  authors = ["Joe"],
  minimal_nickel_version = "1.9.0",
  dependencies = {
    leaf = 'Git { url = "https://example.com/leaf" }
  },
} | std.package.Manifest
