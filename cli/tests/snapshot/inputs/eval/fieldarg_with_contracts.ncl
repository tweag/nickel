# capture = 'stderr'
# command = ['eval', '--field', 'opt_level']

# Regression test for https://github.com/tweag/nickel/issues/1774
{
  OptLevel =
    std.contract.from_predicate
      (
        fun value =>
          std.array.elem value ["O0", "O1", "O2", "O3"]
      ),
  level = 4,
  opt_level | OptLevel = "A" ++ std.string.from_number level,
}
