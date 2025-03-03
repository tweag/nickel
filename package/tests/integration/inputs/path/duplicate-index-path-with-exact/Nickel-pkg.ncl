{
  name = "single-index-dep",
  description = "A package having two differently named dependency paths to the same index package, one of which has an exact constraint",
  version = "0.1.0",
  authors = ["Joe"],
  minimal_nickel_version = "1.9.0",
  dependencies = {
    leaf = 'Index { package = "github/example/leaf", version = "=0.1.1" },
    dep = 'Path "../single-index-compat-dep",
  },
} | std.package.Manifest
