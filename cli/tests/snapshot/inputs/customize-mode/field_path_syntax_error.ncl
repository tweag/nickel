# capture = 'stderr'
# command = ['query']
# extra_args = [
#  '--field',
#  'input.+foo.baz',
# ]
{
  input.foo.bar | String,
  input.foo.baz | Array Number,
  input.defaulted.subfield
    | doc "Some documentation"
    | default = 2,

  override.first = 1,
  override.second = {
    subsecond = {
      subsubsecond = "a",
      other = [],
    }
  }
}
