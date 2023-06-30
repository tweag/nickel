# lsp-harness

This is a testing harness for language servers, together with some snapshot
tests for `nls`.

## The snapshot format

Each snapshot test loads one or more files into `nls`, then runs one or
more requests and checks that the responses are as expected. The input files
are divided up into chunks delimited by lines starting with `###`.
Each chunk between these lines consists of the contents of a file to be loaded into
the language server, and the line before the chunk specifies the name of that file
(which must be an absolute path).

The input file may end with a block of lines starting with `###`. If it does,
these lines specify a list of requests to send to the language server.
These requests are written as LSP requests translated to toml.

For example, a test file might look like:

```text
### /absolute/path.ncl
<contents of /absolute/path.ncl>
### /another/absolute/path.ncl
<contents of /another/absolute/path.ncl>
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///absolute/path.ncl"
### position = { line = 3, character = 8 }
```
