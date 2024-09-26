# capture = 'all'
# command = ['test']
{
  foo
    | doc m%"
    ```nickel
    foo + "1"
    ```

    ```nickel
    foo + "1"
    # => 2
    ```
    "%
    = 1,
}
