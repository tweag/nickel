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
[topiary](https://github.com/tweag/topiary).

To enable formatting in NLS, you have to make the `topiary` executable available
in your `PATH`. Please follow [Topiary's setup
instructions](https://github.com/tweag/topiary#installing) and ensure in
particular that the environment variable `TOPIARY_LANGUAGE_DIR` is correctly set
(this is covered in the setup instructions).
