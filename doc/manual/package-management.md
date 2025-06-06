---
slug: package-management
---

# Package management in Nickel

**Nickel's package management is experimental and subject to change! Official
Nickel releases have package management disabled. You either need to download
package-enabled pre-built binaries (ending in `-pkg)` or to build Nickel from
source with the `--features=package-experimental` flag to enable it.**

As part of its mission to reduce your configuration boilerplate, Nickel allows
importation of external libraries. These libraries can be imported from

- the local filesystem,
- a git repository (local or remote), or
- the global Nickel package index.

We make a distinction here between "importing libraries" and "importing files". In
Nickel, you can import a local file with the `import "path/to/file.ncl"` syntax.
Usually, you will use this kind of local import when the file doing the importing
and the file being imported are part of the same project and managed in the same
source code repository. When we talk about "importing libraries", we're talking
about importing code that's managed and versioned separately from the code
that's doing the importing. As a consequence, Nickel provides tooling to help
ensure that you're importing an appropriate version of a library, and obtaining
it from the right place.

## The manifest file

Nickel's package management requires that you declare up front which libraries
you intend to import. To do this, you write a *manifest file*, which is just
a Nickel file named "Nickel-pkg.ncl" that conforms to the contract
`std.package.Manifest`. For example, your manifest file might look like this:

```nickel
{
  name = "my-github-workflow",
  version = "1.0.0",
  authors = ["Me <me@example.com>"],
  minimal_nickel_version = "1.12.0",
  dependencies = {
    gh = 'Index { package = "github:nickel-lang/github-workflow", version = "1.0.0" }
  },
} | std.package.Manifest
```

This example manifest declares that your local Nickel code depends on the
"github:nickel-lang/github-workflow" package from Nickel's global package
index, and that you will be using the shortcut "gh" to refer to that package.
Then your Nickel code can call `import gh`, and it will evaluate to the contents
of the "github:nickel-lang/github-workflow" package.

When you run `nickel eval` or `nickel export`, Nickel will look for a manifest
file by starting at the directory containing the file you're evaluating and
searching parent directories until it finds a package named "Nickel-pkg.ncl".
So for example, if your filesystem looks like

```text
.
├── Nickel-pkg.ncl
└── src/
    ├── ci.ncl
    ├── release.ncl
    └── utils.ncl
```

and you run `nickel export src/ci.ncl` then Nickel will read the `Nickel-pkg.ncl`
manifest and make available the dependencies that are declared there.
You can change this manifest-finding behavior by using the `--manifest-path` option
to specify the manifest location.

## Package versions and the lock file

When you specify dependency library in your manifest, you also specify a version.
But the library version that Nickel chooses may not always be *identical* to
the one you specify. For example, it could be that you ask for
version 1.0 of "github:nickel-lang/json-schema" but you also depend on
"github:nickel-lang/github-workflow" which depends on version 1.1 of
"github:nickel-lang/json-schema". In this case, Nickel will use version 1.1.

Nickel assumes that all libraries in the global index follow
[semantic versioning](https://semver.org)[^cargo]: versions look
like "1.2.3", in which "1" is the "major version", "2" is the "minor version"
and "3" is the "patch version." Libraries indicate a break in backwards compatibility
by increasing the left-most non-zero version, so "2.0.0" is backwards incompatible
with "1.2.3" and "0.5.0" is backwards incompatible with "0.4.9".

[^cargo] more precisely, pre-1.0 versioning is assumed to follow
[Cargo's](https://doc.rust-lang.org/cargo/reference/manifest.html#the-version-field)
conventions, where the left-most non-zero number determines compatibility.

When you specify a dependency version in your manifest file, Nickel will pick a
version for you that is *at least as large* and *backwards compatible with* the
version you specify. So if you specify "1.2.3", you could end up with "1.2.5" or
"1.3.0", but not "2.0.0". The exact chosen version will depend on whether your other
dependencies introduce their own constraints, but Nickel will always try to find
the [minimal version](https://research.swtch.com/vgo-mvs) that satisfies everyone.

If you wish to specify the *exact* version of some dependency, you can do so
using the syntax "=1.2.3". Unlike some other package managers, Nickel does not
support more complex constraints like ">=1.2.3,<1.3.7".

<!-- TODO: write about the lock file -->

## Publishing your package

If you have a useful Nickel package, please consider publishing it to the global
index! The global index is currently hosted on GitHub; in order to publish a package
it will need to be available on GitHub, and you will also need a GitHub account
in order to submit a PR to the global index.

The package publishing workflow is currently somewhat involved, but we are working
on tooling to improve it:

1. Fork the global index (github.com/nickel-lang/nickel-mine) on github.
2. Clone your fork onto your local machine.
3. The package you want to publish must be at the root of a git repository.
   `cd` into that repository (or supply `--manifest-path` in the next step).
4. Run `nickel package publish --index <directory-of-your-clone> --package-id github:you/your-package`
5. You should see that your local machine's index was modified. Commit that
   modification.
6. Push your package to the `you/your-package` repository on github. These
   names *must* match the package id in the index, and you must ensure that
   the version you push to github matches the SHA-1 hash in the index.
7. Open a pull request to `github.com/nickel-lang/nickel-mine` to make your
   index modifications public.
