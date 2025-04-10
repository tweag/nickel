---
slug: command-line-interface
---

# The Nickel command-line interface

The Nickel command-line interface (CLI) provides many tools for interacting
with Nickel files: evaluating, formatting, testing, and more. You can find
brief documentation for all of these tools in the CLI itself:

```console
$ nickel help

The Nickel interpreter CLI

Usage: nickel [OPTIONS] <COMMAND>

Commands:
  eval             Evaluates a Nickel program and pretty-prints the result

# etc.

$ nickel help eval

Evaluates a Nickel program and pretty-prints the result

Usage: nickel eval [OPTIONS] [FILES]... [-- <CUSTOMIZE_MODE>...]

Arguments:

# etc.
```

This document contains some more in-depth documentation on some of the CLI
commands.

## Customize mode: passing parameters to configurations

**Warning: the CLI customize mode is still experimental and subject to breaking
changes, although it has been there for several minor versions and isn't
expected to radically change.**

The customize mode of the CLI allows to pass values to a Nickel configuration
directly from the command line. It is supported by the `nickel eval` and `nickel
export` commands. After providing the files to evaluate (and potential other
arguments), you can provide a list of key-value assignments that set or override
the top-level fields of the configuration after the `--` separator.

For example, for a file `main.ncl`:

```nickel
{
  params | not_exported = {
    environment | [| 'dev,  'prod |],
    mock_db | Bool,
  },
  test_string = "env=${params.environment}, mock_db=${params.mock_db}",
}
```

You can run:

```console
$ nickel export main.ncl -- params.environment=\'dev params.mock_db=true
{
  "test_string": "env=dev, mock_db=true"
}
~
```

The customize mode only allow to set fields that are not already set in the
configuration (or only have a defaut value). You can override existing values as
well, but you need to use `--override <key=value>`:

```console
$ nickel export main.ncl -- params.environment=\'dev params.mock_db=true --override 'test_string="my own test string, eventually"'
{
  "test_string": "my own test string, eventually"
}
```

The value might be any valid Nickel expression. Note that strings or other
Nickel characters that have a special meaning in the shell need to be properly
escaped.

For a given configuration, you can list the fields available for assignment or
overriding with `list`:

```console
$ nickel export main.ncl -- list
Input fields:
- params.environment: <[| 'dev, 'prod |]>
- params.mock_db: <Bool>

Overridable fields (require `--override`):
- test_string

Use the `query` subcommand to print a detailed description of a specific field. See `nickel help query`.
```

Detailed help on the customize mode is available with `nickel export main.ncl --
help`.

### Sigil expressions

The value part of an assignment supports an extended syntax called sigil
expressions, of the form `@<selector>[/attribute]:<argument>`. Currently, the
only supported selector is `env`, which fetches an environment variable and puts
its value in the corresponding field as an expression:

```console
nickel export myconfig.ncl -- environment.path=@env:PATH enviornment.classpath=@env:CLASSPATH
```

`@env` doesn't support any attribute and fails if the environment variable isn't
set.

We plan to add more selectors in the future, such as `@file` to put the content
of a data file in field, but this isn't yet implemented as of Nickel 1.11.

## `nickel doc`: Generate API documentation

When you create a Nickel code for other people to use or customize, your users
might expect it to be documented. The `nickel doc` command generates markdown
documentation straight from the documentation metadata and contract annotations
in your Nickel source code.

For example, if the file "main.ncl" contains

```nickel
{
  foo
    | Number
    | doc "This is my field named foo."
}
```

then running `nickel doc main.ncl` will create the file `.nickel/doc/main.md` with
the contents

```markdown
# `foo`

- `foo | Number`

This is my field named foo.
```

### The "record spine"

`nickel doc` works only on records: if your file evaluates to a record at the
top level, it will document its fields. If those fields evaluate to records, it
will document the fields of those records, and so on recursively. The recursion
will stop when Nickel finds anything that isn't a field. For example, the following
Nickel file contains a function at the top level. Even though that function
returns a record, `nickel doc` will not evaluate the function and will not document
the record that it returns.

```nickel
fun x => { foo | doc "This won't appear in the output of `nickel doc`" }
```

### Documenting imports

`nickel doc` evaluates imports, and if those imports evaluate to records then it
will recurse into those imports. In practice, when you document a Nickel library
that is implemented across multiple files, you only want to run `nickel doc` on the
main entry point. For example, if "main.ncl" contains

```nickel ignore
{
  main_field | doc "My main field",
  sub_record
    | doc "Other stuff"
    = import "inner.ncl",
}
```

and "inner.ncl" contains

```nickel
{
  inner_field | doc "My inner field",
}
```

then running `nickel doc main.ncl` will document `main_field`, `sub_record`,
and `inner_field`. Since you probably don't expect users to import "inner.ncl"
directly, there's no need to run `nickel doc inner.ncl`.

## `nickel test`: Unit/documentation tests

With the `nickel test` command, Nickel supports using documentation examples as
tests. This command extracts markdown blocks in documentation metadata, and runs
them as Nickel snippets. For example, if the file "main.ncl" contains

````nickel
{
  foo
    | Number
    | doc m%"
    This is my field named foo.

    ## Examples

    ```nickel
      1 + "2"
    ```
    "%
}
````

then running `nickel test main.ncl` will fail with the error message

```console
testing foo/0...FAILED
test foo/0 failed
error: dynamic type error
  ┌─ [..]/test.ncl:1:7
  │
1 │   1 + "2"
  │       ^^^ this expression has type String, but Number was expected
  │
  = (+) expects its 2nd argument to be a Number

1 failures
error: tests failed
```

Only code blocks with the "nickel" tag are tested.

### Testing for expected output

In order to check that a test evaluates to a given value, terminate its
code block with a comment like `# => <expected output>`. This is equivalent
to applying a `std.contract.Equal <expected output>` contract, but might
look better in the rendered documentation. For example, running
`nickel test` on

````nickel
{
  foo
    | Number
    | doc m%"
    This is my field named foo.

    ## Examples

    ```nickel
      1 + 1
      # => 3
    ```
    "%
}
````

will output

```console
testing foo/0...FAILED
test foo/0 failed
error: contract broken by a value
  ┌─ <unknown> (generated by evaluation):1:1
  │
1 │ std.contract.Equal 3
  │ -------------------- expected type
  │
  ┌─ /home/jneeman/tweag/nickel/test.ncl:1:3
  │
1 │   1 + 1
  │   ^^^^^ applied to this expression
  │
  ┌─ <unknown> (generated by evaluation):1:1
  │
1 │ 2
  │ - evaluated to this value

1 failures
error: tests failed
```

### Checking for errors

Sometimes you want a test case to ensure that errors are raised when
appropriate. You can check for this by terminating a code block with
a comment like `# => error: <expected error text>`, and the test runner
will check that the example raises and error, and that the error message
contains "\<expected error text\>" as a substring. To test for an error
without checking the error message, terminate a code block with `# => error`.

For example,

````nickel
{
  foo
    | Number
    | doc m%"
    This is my field named foo.

    ## Examples

    ```nickel
      1 + "2"
      # => error: has type String, but Number was expected
    ```
    "%
}
````

### Ignoring tests

To ignore a test while still keeping the code block tagged as Nickel
source, add the "ignore" label, like

````text
```nickel ignore
1 | String
```
````

### Multiline tests

If you have many short examples, you might prefer not to wrap each
one in a separate code block. In this case, you can use the "multiline"
label to create multiple blank-line-separated tests in a single code block.
For example,

````nickel
{
  foo
    | Number
    | doc m%"
    This is my field named foo.

    ## Examples

    ```nickel multiline
      1 + "2"
      # => error: has type String, but Number was expected

      1 + 1
      # => 2

      1 + 2
      # => 3
    ```
    "%
}
````

### The test expression's environment

Each test expression will be evaluated in the same environment as the field it's
documenting: the name of the field will be in scope, along with the names of all
the sibling fields.

For example, the following tests will succeed:

````nickel
{
  bar
    | Number
    = 2,

  foo
    | Number
    | doc m%"
    This is my field named foo.

    ## Examples

    ```nickel
      foo
      # => 1
    ```

    ```nickel
      foo + bar
      # => 3
    ```
    "%
    = 1,
}
````
