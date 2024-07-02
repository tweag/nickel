---
feature: package management
start-date: 2024-06-28
author: Joe Neeman
---

# Package management

People want to reuse code, but Nickel doesn't currently have a good way to do
it. We should have a way to fetch packages and make them easily available to
Nickel code. The mechanism needs to be predictable (it should fetch the code
that the user expects to fetch) and reliable (if it works on my machine it
should work on your machine).

## The manifest file

We will require a manifest file in order to import packages. Manifest files must
be named `package.ncl`, and they are found by searching up from the file being
evaluated. That is, when the user invokes `nickel export path/to/foo.ncl`, we
look for a manifest at `path/to/package.ncl` and then at `path/package.ncl`, and
so on.

The manifest file format is defined by the contract `std.package.Manifest`,
while is defined as

```nickel
{
  name | String,
  version | Semver,
  nickel-version | Semver,

  dependencies
    | { _:
      [|
        'Path String,
        'Git { url | String, branch | optional | String, rev | optional | String },
        'Index { name | String, version | Semver },
      |]
      }
    | default
    = {},
}
```

So an example manifest might look like

```nickel
{
  # ...
  dependencies = {
    foo = 'Index { package = "github/tweag/foo", version = "1.2.0" }
    bar = 'Path "../my-bar",
  }
} | std.package.Manifest
```

### Alternative: inline dependencies

Nix and Dhall allow for importing dependencies dynamically, using things like
`fetchGit`. There was some discussion
[here](https://github.com/tweag/nickel/issues/329#issuecomment-967372858)
on the advantages and disadvantages of inline imports.

### Alternative: toml manifest

Maybe the manifest should be in some plain-data format like toml. This would
be easier to modify programmatically.

### Alternative: shorthand for registry imports

Cargo allows a shorthand like

```toml
    "github/tweag/foo" = "1.2.0"
```

instead of

```nickel
    foo = 'Index { package = "github/tweag/foo", version = "1.2.0" }
```

Since we expect registry imports to be the common case, maybe it's worth having
a shorthand?

### Question: manifest file name

The name `package.ncl` was chosen to be similar to npm's `package.json` or
stack's `package.yaml`. One problem with this is that it could be confused for
a nickel source file. Another possibility would be to use an extension-based
name like `<package-name>.cabal`. Or just a stranger name that's less likely to
conflict with something real.

## Import statements

The manifest file assigns a name to each dependency; to import the dependency
named `foo` you simply write `import foo`. That is, an `import` statement either
takes a string in quotes -- in which case it imports a path -- or an identifier
without quote -- in which case it imports a package.

The `import foo` expression evaluates to the contents of `main.ncl` in `foo`'s
root directory.

### Question: other entry points?

We've hardcoded `main.ncl` as the entry point of every package, but what if
they want to expose multiple entry points? For example, node allows a package's
manifest to specify the entry point(s). This is probably not very important to
support, as you can just put

```nickel
{
  other = import "./other.ncl",
  blah = import "./blah.ncl",
}
```

in your package's `main.ncl`, to provide "other" and "blah" as other entry points.

## Kinds of dependencies

Where can dependencies come from? Dhall allows imports from arbitrary urls. Nix
supports fetching from a variety of VCSs, paths, and archive formats.

We'll support dependencies from

- a central registry, that can identify packages by name and version number.
  This should be the most common method of importing packages, like `crates.io`
  in rust.
- git repositories (either from HEAD, or from branches, tags, and revisions
  specified by hashes). This allows for easy use of unpublished packages,
  including in-development versions.
- paths (relative or absolute). This allows for easy use of different packages
  within the same repository, or for temporary patching of published packages.

We require packages to have their own manifest file (at the package root), even
if they don't import dependencies.

## Lock files

In order to ensure reproducibility across time and across machines, we build
a lock-file (if there isn't yet one) when running `nickel eval` or `nickel
export`. The lock-file specifies the exact versions of all (transitive)
dependencies, allowing those identical versions to be used every time.

- For a git dependency, the manifest might not specify the exact revision (it
  might specify a branch or tag, or just default to HEAD). The lock-file will
  record the exact revision.
- For a repository dependency, the version specifier might allow for a range of
  versions. The lock-file will record the exact version used.
- For a path dependency, the lock-file will record that there was a path
  dependency, but it won't record anything about it and it will ignore recursive
  dependencies. This is because path dependencies can change at any time, so
  they can't be meaningfully locked.

What happens if we have a lock-file, but we modify the manifest? We don't want
to be too strict about requiring the exact versions in the lock-file, or we'll
end up forcing the user to re-create the lock-file from scratch. In this case,
we treat the lock-file as a suggestion instead of a hard constraint: during
resolving, when choosing the next package version to try, it picks the locked
version first. But if the locked version leads to a conflict, it will try
another version without complaining. If nothing has changed since the lock-file
was created, it should always resolve the same versions.

## Version compatibility and resolution

How do we handle a package that gets imported multiple times in the dependency
tree?

For path and git dependencies, there isn't much choice. Dependencies
from the registry are the most interesting. Fortunately, there are fairly
well-established conventions for specifying ranges of versions (like ">=1.0
<3.0", or "^1.2"). What's less clear is how to handle multiple packages with
overlapping ranges. Some languages (e.g. python) insist that each package
resolves to a single version across the whole dependency tree. Other languages
allow multiple versions, keeping track of which package in the dependency tree
needs to import which version of a package.

I think we want to allow multiple versions of a package; the alternative can be
fragile and annoying. But then we need to figure out how many different versions
to allow. There's a trade-off: if we allow pulling in a different version
every time a package gets imported, solving the dependency graph is easy.
But it increases the chance of getting incompatibilities at runtime: we might
accidentally get a value from `util@1.1` and try to pass it to an incompatible
function defined in `util@1.2`. Pulling in too many different versions also
increases the total number of packages in the dependency graph.

The current prototype uses a strategy similar to cargo: it divides package
versions into semver-delimited "bins" and allows resolution to choose at most
one version from each bin. That is, we can have a `util@2.2` and a `util@1.2` in
the same dependency tree, but not a `util@1.2` and a `util@1.1`.

## The registry

How should we manage the global registry? There's a potential for incurring
substantial maintenance costs here, so we should be careful.

We will provide a git repo, at a hard-coded location, to serve as the registry.
This repo will contain the "index", but not the actual package contents. It
will contain one file per package, each of which contains a line per version.
Each entry specifies the location of the package (currently required to be on
github) and its git tree hash. This ensures that packages are immutable, but it
doesn't stop them from disappearing: we don't keep a copy of the actual package
contents.

The registry entries are named like "github/\<org\>/\<package\>" (where in the
future we might support places other than github). This allows the package
registry to automatically discover new package versions: to find the latest
versions of "github/tweag/json-schema-lib", we simply fetch the repository at
`github.com/tweag/json-schema-lib` and look for tags that look like version
numbers. Initially, we will scrape packages daily in a cron job. Eventually we
will allow people to automatically request re-scrapes of specific packages.

Once a package version is stored in the index, it will never be overwritten.
If a future scrape sees that a previously existing version tag is pointing at a
different commit, we will make a note (maybe warn someone somehow?) and keep the
old version.

### Question: should we store a content hash too?

We're storing a git tree hash in the index, but if we ever want to store package
contents in the future, maybe we should also store a hash of the git tree
contents? This would allow verification of package tarballs, without needing the
whole git repo.

## CLI support

We'll need some CLI commands for handling common package-management tasks. The
current prototype has

- a `nickel package generate-lockfile` command that updates the lock-file
- a `nickel package debug-resolution` command that prints the full recursive
  dependency tree

We probably also want

- a command for adding a new dependency to the manifest (checking if it exists,
  and picking the most recent version)
- a command for downloading the dependency tree (for use in build systems that
  expect different "fetch" and "build" phases)
- a command that checks for new dependency versions and updates the manifest

Anything else?
