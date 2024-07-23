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

Explicit non-goals are:

- Management of system (or other non-Nickel) dependencies. You can use Nix for
  that.
- Any sort of processing or compilation. Nickel is an interpreted language, so
  package management is only in charge of distributing unmodified source files.
- Integration with other language ecosystems; this is only for Nickel code.
  Anyone integrating Nickel with other languages should use a polyglot build
  system like Bazel. (However, providing integration points for other tools *is*
  in scope. More detail on that below.)

## The manifest file

We will require a manifest file in order to import packages. Manifest files
must be named `electroplate.ncl`, and they are found by searching up from the
file being evaluated. That is, when the user invokes `nickel export path/to/
foo.ncl`, we look for a manifest at `path/to/electroplate.ncl` and then at
`path/electroplate.ncl`, and so on.

The manifest file format is defined by the contract `std.package.Manifest`,
which is defined as

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
  name = "demo"
  version = "0.1.0"
  nickel-version = "^1.0"
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
be easier to modify programmatically, and it would prevent people from putting
lots of complicated code in their manifest files.
The disadvantage of a plain-data format is that you don't get a nice contract
for it and you can't use ADTs for the dependencies.

We discussed this point in office hours, and the general sentiment was that
it's ok to allow the manifest to be interpreted. If someone wants to use
that power to create a ridiculously complicated manifest, that's their problem.

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

### Alternative: manifest file name

Bikeshed the name "electroplate.ncl".

## Import statements

The manifest file assigns a name to each dependency; to import the dependency
named `foo` you simply write `import foo`. That is, an `import` statement either
takes a string in quotes -- in which case it imports a path -- or an identifier
without quote -- in which case it imports a package.

The `import foo` expression evaluates to the contents of `main.ncl` in `foo`'s
root directory.

### Alternative: other entry points

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

### Alternative: entry points are top-level files

Instead of hardcoding `main.ncl`, we could say that every file in the package's
top-level directory is publicly accessible. Package authors could keep implementation
details private by putting code in subdirectories.

### Alternative: namespace the import tool

Our initial intention for packaging was to allow for the usage of multiple
different package management tools. This RFC only proposes one such tool, but
maybe the import syntax could be designed with other tools in mind. For example,
it could be `import foo from electroplate` with the idea that future nickel
versions might add, say, `import foo from nix-flake`.

## CLI interface

We will add a new CLI tool (called `plate`) that wraps the nickel CLI and
adds package management. It will offer a superset of the nickel CLI's commands
and arguments. For example, `plate eval foo.ncl` is the same as `nickel eval
foo.ncl`, except that it reads the manifest file, prepares the dependencies, and
makes them available to the nickel interpreter before evaluating.

### Alternative: built-in package management

We could build package management straight into the nickel CLI. This might be
more convenient and more discoverable, but it comes with stability hazards:
we might want to evolve the `plate` interface more rapidly than the `nickel` one.
Building in package management would also bloat the nickel CLI.

### Alternative: two-stage interface

Rather than wrapping the original `nickel` CLI, the `plate` command could
*prepare* the packages for `nickel`, which would be in charge of loading
them. The workflow would then be `plate install` followed by `nickel eval foo.ncl`.
This would be more like how `npm` or `poetry` work (whereas the wrapping
interface is more similar to `cargo`).

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

## Restrictions on path dependencies

Path dependencies can be problematic for reproducibility, because they require
something to be present at a given path. In order to mitigate this:

- Packages imported from the index are not allowed to have path dependencies.
- Packages imported from git can have path dependencies, but only if they point
  within the same git repo. The lock-file treats these path dependencies as though
  they were git dependencies.

Therefore your dependency tree can have a few path-dependency subtrees, but only
at the root.

## Stale lock files

What happens if there's a lock-file, but someone modifies the manifest? The
lock-file might need to be regenerated: certainly it might need some new
entries, but also there might be new version conflicts that require a different
resolution. In this case, we treat the lock-file as a suggestion instead of a
hard constraint: during resolving, when choosing the next package version to
try, we try to pick the locked version first. But if we run into a resolution
conflict, we allow a different version to be chosen (and notify the user
that a package was changed).

This behavior is similar to what cargo does. It has the advantage that if the
new manifest is compatible with the old lock-file, nothing will be changed.

### Alternative: interactive prompt

Instead of merely notifying the user that the lock-file changed, we could
require them to approve the changes.

### Alternative: explicit regeneration

Instead of automatically updating the lock-file when the manifest changes,
we could update it only on explicit commands: `plate eval` would use an
old lock-file if it exists, while `plate lock` would read the manifest
and update the lock-file. This alternative is potentially more efficient
(as `plate eval` wouldn't need to re-read the manifest, and the manifests
of path-dependencies, on every invocation), but makes it easy to accidentally
use out-of-date packages.

## Version compatibility and resolution

How do we handle a package that gets imported multiple times in the dependency
tree?

For path and git dependencies, there isn't much choice. Dependencies from the
registry are the most interesting.  Some languages (e.g. python) insist that
each package resolves to a single version across the whole dependency tree.
Other languages allow multiple versions, keeping track of which package in the
dependency tree needs to import which version of a package.

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

### Question: how to handle pre-1.0 minor versions?

Officially, semver says that pre-1.0 versions are mutually incompatible. If we
follow this, pre-1.0 versions would never get binned together. `cargo` modifies
the semver rules, allowing `0.x.y` versions to be binned together if they share
the same `x`. Should we do the same?

## The registry

How should we manage the global registry? There's a potential for incurring
substantial maintenance costs here, so we should be careful.

We will provide a git repo, hard-coded to live at `github.com/tweag/nickel-mine`,
to serve as the registry.
This repo will contain the "index", but not the actual package contents. It
will contain one file per package, each of which contains a line per version.
Each entry specifies the location of the package (currently required to be on
github) and its git tree hash. This ensures that packages are immutable, but it
doesn't stop them from disappearing: we don't keep a copy of the actual package
contents.

The registry entries are named like "github/\<org\>/\<package\>" (where in the
future we might support places other than github). This allows us to skip
registration and authentication: if someone has the github permissions to create
`jneem/foo` on github, they also have permissions to create and update the
`github/jneem/foo` nickel package.

We will provide a backend service to update packages. Users can submit a request
to the backend, asking to publish version `0.6.5` of package `jneem/foo` at a
specific git revision. The service will check that the github repo `jneem/foo`
has a `v0.6.5` tag pointing at that git revision; if so, it will add it to
the index.

### Alternative: manual pull requests

If package volume is low enough (which it probably is, at first), index updates
could be done manually via pull requests.

### Question: should we store a content hash too?

We're storing a git tree hash in the index, but if we ever want to store package
contents in the future, maybe we should also store a hash of the git tree
contents? This would allow verification of package tarballs, without needing the
whole git repo.

## Integration point: package maps

Other tools (like build systems) may need to (1) consume the dependency resolution
that we produce, or (2) plug in their own dependencies to the nickel interpreter.
For (1), they can consume our lock-file, which will be in JSON. This means its
format needs to be stable.

For (2), we divide the package management implementation into two parts: the `plate`
command has all the logic for consuming the manifest, fetching packages, and so on.
The other part is to teach the nickel interpreter about "package maps," which is
just a map associating a filesystem path to each pair
`(package filesystem path, package-local name)`. When the nickel interpreter
is running a file that came from the package at path `/home/jneem/foo`, and that
file contains `import bar`, the nickel interpreter looks up `(/home/jneem/foo, bar)`
in the package map to figure out where to import from.

After `plate` fetches dependencies, it provides the interpreter with the correct
package map to find them. Other tools with their own dependency-fetching methods
can invoke the interpreter with a custom package map to makes those packages
available.

## CLI support

We'll need some CLI commands for handling common package-management tasks. The
current prototype has

- a `plate package generate-lockfile` command that updates the lock-file
- a `plate package debug-resolution` command that prints the full recursive
  dependency tree

We probably also want

- a command for adding a new dependency to the manifest (checking if it exists,
  and picking the most recent version)
- a command for downloading the dependency tree (for use in build systems that
  expect different "fetch" and "build" phases)
- a command that checks for new dependency versions and updates the manifest

Anything else?
