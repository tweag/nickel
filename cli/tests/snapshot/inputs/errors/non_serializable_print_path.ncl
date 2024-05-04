# capture = 'stderr'
# command = ['export']

# Check that a non-serializable error prints the path in the notes. This is
# particularly useful in the case tested below, when we haven't fully applied a
# contract, because the error points to the contract definition site (at the
# time of writing), and not the usage site, which is not very useful.
let SomeParametricContract = fun parameter label value => value
in
{
  foo.bar.baz =
    [0, 1]
    @ [
      2,
      (
        {
          inner = { qux_miss_param | SomeParametricContract = {} },
        }
        & { inner.qux_without_error = 1 }
      ),
    ],
}
