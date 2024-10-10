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
  nickel-version
    | Semver,
    | doc "The minimum version of nickel supported by this package",


  dependencies
    | { _:
      [|
        'Path String,
        'Git { url | String, branch | optional | String, rev | optional | String },
        'Index { name | String, version | SemverConstraint },
      |]
      }
    | default
    = {},
}
```

So an example manifest might look like

```nickel
{
  name = "demo",
  version = "0.1.0",
  nickel-version = "1.0",
  dependencies = {
    foo = 'Index { package = "github/tweag/foo", version = "1.2.0" },
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

### Alternative: specify the entrypoint in the manifest

Since the package manifest is a nickel file, it could contain an arbitrary nickel
expression as its entrypoint. That is, the `import foo` expression could evaluate
to `(import "<path-to-foo>/electroplate.ncl").entrypoint`, and we could provide
a default `entrypoint | default = import "main.ncl"` manifest entry. It might
require some care to avoid looking for the "main.ncl" file if it doesn't exist.

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
details private by putting code in subdirectories. The import syntax would
need to change somehow to specify the entry point.

### Alternative: namespace the import tool

Our initial intention for packaging was to allow for the usage of multiple
different package management tools. This RFC only proposes one such tool, but
maybe the import syntax could be designed with other tools in mind. For example,
it could be `import foo from electroplate` with the idea that future nickel
versions might add, say, `import foo from nix-flake`.

One problem with this approach is that even if you switch package managers, you
still want to import the same packages. So probably it doesn't make sense to
include the name of the package manager in the nickel source.

## CLI interface

We will build package management straight into the nickel CLI. `nickel eval`,
`nickel export`,
and similar commands will do an additional package-management step before the
actual evaluation. They will start by searching for an `electroplate.ncl` file.
If one is found, we will evaluate it. We will then search for a lock-file.
If one is found, it will be used to guide dependency resolution; if not, we will
do dependency resolution from scratch and write out the generated lock-file.

Once dependencies are resolved, they will be downloaded if necessary
(git dependencies will need to be downloaded during resolution), and then
cached. Finally, the nickel interpreter will be invoked with the data
necessary to find the downloaded dependencies (see the section on "package
maps" below).

There will be command line flags to fine-tune this behavior. For example, there
could be a `--locked` flag that triggers a failure if the lock-file is not
present and up-to-date, or an `--offline` flag that triggers a failure if the
dependencies aren't already available. There could also be a flag (`--no-electroplate`?)
to disable package-management altogether.

### Alternative: a separate CLI tool

We could add a new CLI tool (called `plate`) that wraps the nickel CLI and
adds package management. It would offer a superset of the nickel CLI's commands
and arguments. For example, `plate eval foo.ncl` is the same as `nickel eval
foo.ncl`, except that it reads the manifest file, prepares the dependencies, and
makes them available to the nickel interpreter before evaluating. This would
allow package-management to be opt-in.

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

### Alternative: global snapshots à la Stackage

Maybe we can just avoid version resolution altogether? For example, Stackage
provides an ecosystem-wide snapshot of packages that are mutually compatible.
Then users just depend on a single version of Stackage, and all other versions
are determined from there (with some escape hatches if they need a specific
version of a specific package). nixpkgs works similarly; a single revision
of nixpkgs fixes the versions of all the packages in nixpkgs.

We might have some trouble automatically coming up with mutually compatible
snapshots, though. Haskell gets to use compile-time checks to test compatibility,
and nixpkgs just does a lot of building and testing to check whether everything
works. Nickel being dynamic and lazy might make this hard.

### Alternative: minimal version selection

Go uses a system called "[minimal version selection]" in which all version
updates are required to be backwards-compatible: if you want to break some
API, you have to add the new API at a different path and retain the old API.
Then version resolution is trivial: just take the oldest version that is
compatible with all the requirements. Advantages:

- version resolution is trivial
- version specification is trivial (there are no version ranges, just lower bounds)
- version resolution is stable over time, even without a lock file; resolving
  to the oldest compatible version means that we don't change resolution just
  because a new version became available

I don't have much experience using this method, but it seems like one
disadvantage is that it puts an extra burden on package maintainers to keep
backwards compatibility even across "major" version bumps.

[bzlmod] also uses some version of minimal version selection, but they
do apparently allow for backwards-compatibility breakage. They have
an extra piece of metadata called "compatibility level" (not encoded in
the version number). They don't allow a dependency graph to contain different
compatibility levels of the same module.

[minimal version selection]: https://research.swtch.com/vgo-mvs
[bzlmod]: https://docs.bazel.build/versions/5.0.0/bzlmod.html

### Some interpolation between MVS and semver

There are some middle grounds between "minimal version selection" and
cargo-style resolution. We could follow semver and allow multiple incompatible
versions of packages, while also doing one or both of the following

- preferring to resolve minimal versions instead of maximal versions. This promotes
  stability in version resolution, at the cost of requiring intervention to bring
  in bugfixes in dependencies.
- only allowing `^` version specifications (along with `=` overrides at the top level).
  This amounts to being opinionated that "the ecosystem is using semver, and your
  version requirements should comply with semver." It's basically a semver analogue
  of go's policy of only allowing lower bounds (plus pinning at the top level).

### Question: how to handle pre-1.0 minor versions?

Officially, semver says that pre-1.0 versions are mutually incompatible. If we
follow this, pre-1.0 versions would never get binned together. `cargo` modifies
the semver rules, allowing `0.x.y` versions to be binned together if they share
the same `x`. Should we do the same?

## Yanking packages

What if someone messes up and publishes a package that's incompatible with
an earlier version? In this case, we allow packages in the registry to be
"yanked," meaning that they are left there but marked as broken. The resolver
will ignore yanked packages unless they are already in the lock-file.
This means that if you're happily using a package version that got yanked
(maybe you're not using the broken part of the package) then you won't
get broken by the yank.

Package yanking will be done with the same process as package publishing
(i.e., probably PRs to the index). See the next section.

### Alternative: allow yanked packages if resolution would otherwise fail

A slightly more permissive alternative would be to ignore yanked packages that
aren't in the lock-file, but if resolution fails then go back and try allowing
them. This would allow resolution to succeed, for example, if your manifest
asks for the latest version of a package *and* that package was yanked *and*
you don't have a lock-file. But this seems like maybe adding too many corner-cases.

## The registry

How should we manage the global registry? There's a potential for incurring
substantial maintenance costs here, so we should be careful.

We will provide a git repo, hard-coded to live at `github.com/nickel-lang/nickel-mine`,
to serve as the registry.
This repo will contain the "index", but not the actual package contents. It
will contain one file per package, each of which contains a line per version.
Each entry specifies the location of the package (currently required to be on
github) and its git tree hash. This ensures that packages are immutable, but it
doesn't stop them from disappearing: we don't keep a copy of the actual package
contents.

The registry index will also contain package metadata: the dependencies (so
that we can resolve recursive dependencies without fetching the packages),
and whatever metadata we want to be searchable.

The registry entries are named like "github/\<org\>/\<package\>" (where in the
future we might support places other than github). This allows us to skip
registration and authentication: if someone has the github permissions to create
`jneem/foo` on github, they also have permissions to create and update the
`github/jneem/foo` nickel package.

Packages will be updated by pull requests to the index. There can be some tooling
in the nickel CLI to automate the creation of these pull requests. If the volume
of updates becomes high enough, we can automate the merging.

### Question: should we store a content hash too?

We're storing a git tree hash in the index, but if we ever want to store package
contents in the future, maybe we should also store a hash of the git tree
contents? This would allow verification of package tarballs, without needing the
whole git repo.

## Alternate registries

It's very convenient to have a default global registry, but you might also want
to get the benefits of a registry without making your code public. Therefore
we should support alternatives to the global registry. There are at least two
distinct use-cases:

1. replacing the default registry with an alternative, for example in order to
   host or cache it locally
2. providing an additional registry with private code, to be used alongside
   the global registry

These two use-cases should be configured differently, because the first is
a global setting and the second is per-package. For the global setting, we
add a `source-replacement` field to the manifest: `std.package.Manifest` becomes

```nickel
{
  name | String,
  # ...

  source-replacement | {
    registry | { _ : String },
    git | { _ : String },
  },
}
```

The idea is that you say

```nickel
source-replacement.registry."https://github.com/nickel-lang/nickel-mine.git"
  = "/path/to/a/local/git/repo"
```

or

```nickel
source-replacement.registry."https://github.com/nickel-lang/nickel-mine.git"
  = "https://my-site/nickel-mine"
```

You can also replace git repository locations, because why not.

For per-package alternate registries, we just add a `registry` field to the
package spec: the contract on `dependencies` becomes

```nickel
{ _:
  [|
    'Path String,
    'Git { url | String, branch | optional | String, rev | optional | String }
    'Index { name | String, version | SemverConstraint, registry | optional | String },
    #                                                  NEW! ^^^
  |]
}
```

This is a bit different from what cargo does (it associates registry names to
registry urls in global config, then you use only the *name* in the per-package
part). But since our manifest format is nickel, you can just do that anyway:

```nickel
let my-repo = "https://github.com/jneem/my-mine" in
{
  dependencies = { bar = 'Index { name = "github/foo/bar", version = "0.1.0", registry = my-repo } }
}
```

## Integration point: package maps

Other tools (like build systems) may need to (1) consume the dependency resolution
that we produce, or (2) plug in their own dependencies to the nickel interpreter.
For (1), they can consume our lock-file, which will be in JSON. This means its
format needs to be stable.

For (2), we divide the package management implementation into two parts: the
`nickel_lang_package` crate
has all the logic for consuming the manifest, fetching packages, and so on.
The other part is to teach the nickel interpreter about "package maps," which is
just a map associating a filesystem path to each pair
`(package filesystem path, package-local name)`. When the nickel interpreter
is running a file that came from the package at path `/home/jneem/foo`, and that
file contains `import bar`, the nickel interpreter looks up `(/home/jneem/foo, bar)`
in the package map to figure out where to import from.

When running `nickel eval` or `nickel export`, by default the nickel CLI will
first use the `nickel_lang_package` crate to resolve and fetch dependencies.
Then it provides the interpreter with the correct package map to find them.
Other CLI commands can split up these steps (e.g., only resolving and printing
dependencies; or invoking the interpreter with a custom package map that uses,
say, vendored dependencies).

## CLI support

We'll need some CLI commands for handling common package-management tasks. The
current prototype has

- a `plate package generate-lockfile` command that updates the lock-file
- a `plate package debug-resolution` command that prints the full recursive
  dependency tree

We probably also want

- a command for adding a new dependency to the manifest (checking if it exists,
  and picking the most recent version)
- a command for listing (in machine-readable format) the dependency tree and all
  its relevant metadata (for use in build systems that expect different "fetch"
  and "build" phases)
- a command that checks for new dependency versions and updates the manifest
- a command for automating the pull request necessary for updating your package

Anything else?

## Where do the packages live?

Index and git dependencies need to be downloaded before they are used. We will
use a single global cache directory for all dependencies. It will use the
[`directories`](https://docs.rs/directories/latest/directories/struct.ProjectDirs.html#method.cache_dir)
crate to deduce a platform-appropriate location (`~/.cache/nickel-lang/` on
linux, unless `$XDG_CACHE_HOME` is set). Within this directory, git checkouts
will be stored in directories whose name includes the commit hash. So for
example, the `foo` package could be stored in the `abcdef9876543210-foo`
directory. If the same checkout of `foo` is used multiple times in different
nickel projects, those projects will use the same checkout.

The git checkouts will be created atomically (by first creating them under a temporary
name and then renaming them) and marked read-only, so that multiple nickel processes
can add cached packages without getting in one another's way.

### Alternative: per-project cache

We could use a per-project cache instead of a system-wide cache. This would have
better isolation (e.g. if someone messes with the cache, it doesn't cause problems
for *all* your nickel projects), but it also uses more bandwidth and disk space.
