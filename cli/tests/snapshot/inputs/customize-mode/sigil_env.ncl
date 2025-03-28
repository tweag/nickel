# capture = 'stdout'
# command = ['export']
# extra_args = [
#  '--',
#  'env.pkg=@env:CARGO_PKG_NAME',
# ]

# We bet that CARGO_PKG_NAME is always set in the test environment by cargo, and
# that it's unlikely to change often, which should fit the bill for this
# snapshot test.
{
  env.pkg | String,
} 
