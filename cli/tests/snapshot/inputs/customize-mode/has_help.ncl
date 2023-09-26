# capture = 'stdout'
# command = ['export']
# extra_args = ['--', '--help']
{
  help | String,
  input | Number,

  output = if input == 0 then help else "",
}
