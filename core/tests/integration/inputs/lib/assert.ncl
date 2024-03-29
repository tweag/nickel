# test.type = 'skip'
{
  Assert | doc m%"
    A contract that checks if its argument is the boolean true.

    We could get by just using arrays of boolean tests and `array.all`.

    However, we apply this contract to each test instead because it provides
    fine-grained error reporting, pinpointoing the exact expression that failed
    in an array of tests, as opposed to the pure boolean solution.
    "%
    = fun l x => x || %blame% l,
  # We can't use a static type because x | Assert is not of type bool.
  # We could use additional contracts to make it work but it's not worth the
  # hassle.
  check | doc m%"
    unchecked type: Array Bool -> Bool

    Check an array of tests. Apply the `Assert` contract on each element of the
    given array.

    `check tests` It is semantically equivalent to

    ```nickel
    (array.all function.id tests) | Assert
    ```

    But will give more precise error reporting when one test fails.
    "%
    = std.array.fold_left (fun acc test => (test | Assert) && acc) true,
}
