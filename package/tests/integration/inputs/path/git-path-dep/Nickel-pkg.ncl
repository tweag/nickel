{
  name = "git-path-dep",
  description = "A package whose deps live in subdirs of a git repo",
  version = "0.1.0",
  authors = ["Joe"],
  minimal_nickel_version = "1.9.0",
  dependencies = {
    git-root = 'Git { url = "https://example.com/with-subdirs" },
    git-subdir = 'Git { url = "https://example.com/with-subdirs", path = "subdir-with-path-dep" },
  },
} | std.package.Manifest
