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

The input file must also contain at least one trailing line starting with `###`,
and each such trailing line denotes a request to send to the language server.
The format of these requests is ad-hoc and will be expanded as more requests are
supported. See the implementation of `Parse` for `lsp_harness::Request`.

For example, a test file might look like:

```text
### /absolute/path.ncl
<contents of /absolute/path.ncl>
### /another/absolute/path.ncl
<contents of /another/absolute/path.ncl>
### GotoDefinition /absolute/path.ncl:0:0
```
