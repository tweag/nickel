# capture = 'all'
# command = ['test']
{
  foo
    | doc m%"
    ```nickel
    foo
    # => error
    ```

    ```nickel
    foo + "1"
    # => error: wrong message
    ```

    There can be multiple tests in a single code block

    ```nickel multiline
    foo
    # => error

    1
    # => error
    ```
    "%
    = 1,
}
