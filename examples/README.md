# Examples

This directory contains an evolving selection of examples of Nickel programs.

## Execution

Please follow the main repository's README instructions to have a working nickel
executable. You can then either use the command line or launch an interactive
session. Play with the values to see contracts failing when fed with invalid
data!

### From the command line

- *Base values*: some examples just return numbers, strings or booleans. You can
  run them directly:

    ```console
    $ nickel -f fibonacci.ncl
    55
    ```

- *Configurations*: some return records representing configuration. Nickel won't
  currently print a very readable result as is. Please use the `export`
  subcommand to see the result serialized as JSON:

    ```console
    $ nickel -f merge-main.ncl export
    {
      "firewall": {
      ...
    }
    ```

    Alternatively, you can query a configuration or a sub-element to get
    a list of attributes or the documentation:

    ```console
    $ nickel -f record-contract.ncl query kind
      • contract: <ReplicationController, ReplicaSet, Pod>
      • value: `ReplicationController
      • documentation: The kind of the element being configured.
    ```

### From the REPL

First start the REPL:

```console
$ nickel repl
nickel>
```

- Just import a file directly to evaluate it:

    ```text
    nickel> import "fibonacci.ncl"
    55

    nickel>
    ```

- Use `builtin.serialize` to have the same behavior as the `export` subcommand
  and print the result as JSON:

    ```text
    nickel> builtin.serialize `Json (import "merge-main.ncl")
    "{
        \"firewall\": {
        ...
    }"

    nickel>
    ```

- Use `:query` to retrieve information and documentation about a field:

    ```text
    nickel>let config = import "record-contract.ncl"
    nickel>:query config.kind
    • contract: <ReplicationController, ReplicaSet, Pod>
    • value: `ReplicationController
    • documentation: The kind of the element being configured.

    nickel>
    ```
