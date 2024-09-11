# capture = 'all'
# command = ['test']
{
  foo
    | doc m%"
    Here is some documentation with a test.

    ```nickel
    foo
    ```

    The next test actually checks the value.
    ```nickel
    foo
    # => 1
    ```

    The trailing block can have multiple lines.
    ```nickel
    foo
    # => (
    # 1
    # )
    ```

    There can be multiple tests in a single code block
    ```nickel multiline
    foo
    # => 1

    1
    # => 1
    ```
    "%
    = 1,

  bar
    | doc m%"
    ```nickel
    foo + "1"
    # => error: this expression has type String, but Number was expected
    ```

    We can also test for an error without specifying the message.
    ```nickel
    foo + "1"
    # => error
    ```
    "%
}
