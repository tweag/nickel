# capture = 'stderr'
# command = ['package', 'lock']
# is_package_manifest = true
{
  name = "package",
  version = "0.1.0",
  minimal_nickel_version = "1.10.0",
  authors = [],
  dependencies = {
    git = 'Git { url = "https://does not parse" }
  }
}
