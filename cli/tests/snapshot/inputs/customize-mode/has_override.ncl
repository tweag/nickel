# capture = 'stdout'
# command = ['export']
# extra_args = ['--', '--help']
{
  override | String,
  input | Number,

  output = if input == 0 then override else "",
}
