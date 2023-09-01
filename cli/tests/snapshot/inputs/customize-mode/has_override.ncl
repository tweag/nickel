# capture = 'stdout'
# command = ['export', '--', '--help']
{
  override | String,
  input | Number,

  output = if input == 0 then override else "",
}
