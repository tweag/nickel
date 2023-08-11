# Nickel Language Server

The Nickel Language Server (NLS) is a [language
server](https://en.wikipedia.org/wiki/Language_Server_Protocol) for the
[Nickel](https://www.nickel-lang.org/) programming language. NLS offers error
messages, type hints, and auto-completion right in your favorite LSP-enabled
editor.

NLS is a stand-alone binary. Once built, you must then configure you code editor
to use it for Nickel source files. This document covers building NLS and using
it in VSCode, (Neo)Vim and Emacs.

## Formatting Capabilities

Formatting in `nls` is currently based on
[Topiary](https://github.com/tweag/topiary). NLS exposes a formatting capability
when compiled with the `format` Cargo feature, which is enabled by default. No
configuration or external dependencies are necessary.
