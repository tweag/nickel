{
  name = "duplicate-lock-name",
  description = "A package having two different identically-named path packages in its dep graph",
  version = "0.1.0",
  authors = ["Joe"],
  minimal_nickel_version = "1.9.0",
  dependencies = {
    leaf = 'Path "../single-path-dep"
  },
} | std.package.Manifest
