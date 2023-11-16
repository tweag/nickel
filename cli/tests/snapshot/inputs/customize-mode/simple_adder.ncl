# capture = 'stdout'
# command = ['export']
# extra_args = ['--', 'input=5']
{
  input | Number,
  output = input + 1,
}
