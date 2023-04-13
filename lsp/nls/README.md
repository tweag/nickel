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

Formatting in `nls` is currently based on [topiary](https://github.com/tweag/topiary).

To use it successfully, you need ensure you follow these steps:

1. Have the `topiary` binary installed in your `PATH`. See [here](https://github.com/tweag/topiary#installing).
2. Have the [topiary](https://github.com/tweag/topiary) repo cloned locally.
3. Set the environment variable `TOPIARY_REPO_DIR` to point to the local copy
of the repo.
4. And finally, an environment variable `TOPIARY_LANGUAGE_DIR` to point to `$TOPIARY_REPO_DIR/languages`.

Steps 2-4 are necessary because, for now, `topiary` cannot be used outside its
repo directory.

## Alternatives

I think making a user fetch the `topiary` and set those environment variables,
just to have the formatting capability in `nls` is a bit too much. I can think
of the following alternatives, but I don't know if they are ideal.

### Keep a cache of the topiary repo

Keep a cache of the `topiary` repo. Fetch the repo from GitHub if it is not
available in the local cache or not up to date.

* Pros
  * Automatic updates of the repo (and hence the formatting rules for Nickel)
* Cons
  * We still have to set environment variables at runtime
  * `nls` has to download a potentially large repo

### Embedded Nickel formatting rules as a string in the `nls` binary

 Since `topiary` just needs a single `nickel.scm` to be able to format nickel
 files we could just point `TOPIARY_LANGUAGE_DIR` to `<temp-directory>/language`
 , and put the embedded file in that directory.

* Pros:
  * It's just a single file, so it will be small
  * No need to download/fetch anything
* Cons:
  * We still have to set environment variables at runtime
  * We have to ensure the formatting rules are up to date with `topiary`
