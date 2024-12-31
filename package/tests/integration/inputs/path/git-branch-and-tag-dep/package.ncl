{
  name = "branch-and-tag-git-dep",
  description = "A package depending on a git branch and a git tag",
  version = "0.1.0",
  authors = ["Joe"],
  minimal_nickel_version = "1.9.0",
  dependencies = {
    branch = 'Git { url = "https://example.com/branch-leaf", ref = 'Branch "cành" },
    tag = 'Git { url = "https://example.com/tag-leaf", ref = 'Tag "mytag" },
  },
} | std.package.Manifest
