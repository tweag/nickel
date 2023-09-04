# capture = 'stdout'
# command = ['export', '--', '--help']
{
  help | String,
  input | Number,

  output = if input == 0 then help else "",
}
