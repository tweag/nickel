# capture = 'stdout'
# command = ['pprint-ast']
{
  field | doc m%%"
    Contract to enforce the value is a string that represents a boolean literal. Additionally casts "True" to "true"
    and "False" to "false". This shouldn't interpolate: %{null}

    For example:
    ```nickel
      ("True" | BoolLiteral) =>
        "true"
      ("hello" | BoolLiteral) =>
        error
      (true | BoolLiteral) =>
        error
    ```
    "%%
    = 1
}
