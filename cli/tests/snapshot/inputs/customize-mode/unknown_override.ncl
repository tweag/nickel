# capture = 'stderr'
# command = ['export']
# extra_args = [
#  '--',
#  'input.foo.bar="hello"',
#  'input.foo.baz=[]',
#  '--override',
#  'unknown.field.path=null',
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
