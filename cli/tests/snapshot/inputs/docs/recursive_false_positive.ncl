# capture = 'stdout'
# command = ['doc', '--stdout']

# Companion test for the `recursive` regression test
# Check that the infinite recursion detection of `nickel doc` doesn't have
# obvious false positive
{
  outer = {
    mid = { inner | doc "this is inner" },
    z | doc "this is z" = outer.mid,
  }
}
