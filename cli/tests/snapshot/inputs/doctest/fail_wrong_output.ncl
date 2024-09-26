# capture = 'all'
# command = ['test']
{
  foo
    | doc m%"
    ```nickel
    foo
    # => 2
    ```

    The output block can be split over multiple lines.

    ```nickel
    foo
    # => (
    # 2
    # )
    ```

    There can be multiple tests in a code block.

    ```nickel multiline
    foo
    # => 2

    foo + 1
    # => 3
    ```
    "%
    = 1,
}
