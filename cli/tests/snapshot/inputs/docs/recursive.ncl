# capture = 'stdout'
# command = ['doc', '--stdout']

# Regression test for https://github.com/tweag/nickel/issues/1967 (`nickel doc`
# shouldn't overflow the stack on recursive data)
{
  Recursive = {
    foo | Number,
    bar | Recursive | optional
  }
}
