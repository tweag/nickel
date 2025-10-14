{
  name = "multiple-versions",
  description = "A package with index dependencies of multiple versions",
  version = "0.1.0",
  authors = ["Joe"],
  minimal_nickel_version = "1.9.0",
  dependencies = {
    leaf010 = 'Index { package = "github:example/leaf", version = "0.1.0" },
    leaf020 = 'Index { package = "github:example/leaf", version = "0.2.0" },
    subdir = 'Index { package = "github:example/leaf/subdir", version = "0.2.0" },
  },
} | std.package.Manifest
