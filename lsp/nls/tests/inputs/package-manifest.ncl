### /main.ncl
{
  minimal_nickel_version = "1.10.0",
  dependencies = {
    foo = 'Index { pack, version = "1.2.0" },
  }
} | std.package.Manifest
### [[request]]
### type = "Completion"
### textDocument.uri = "file:///main.ncl"
### position = { line = 3, character = 23 }
###
### [[request]]
### type = "Hover"
### textDocument.uri = "file:///main.ncl"
### position = { line = 1, character = 10 }
