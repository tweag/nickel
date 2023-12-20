# Contributing to Nickel

Welcome, and thanks for considering contributing to the Nickel project!

A great first step is to join our
[![Discord](https://img.shields.io/badge/Discord-100000?style=flat&logo=Discord&logoColor=C3C3C3&labelColor=4179DA&color=010101)][discord]
server.

## Contributing

There are many useful ways to help which don't involve modifying the source of
Nickel:

- Improving the documentation, user-facing or internal, technical or high-level
- Growing the Nickel ecosystem, by providing the community with libraries
  (e.g. a collection of contracts for new use cases), augmenting the
  standard library, or improving the tooling (such as code editor integration).
- Reviewing changes from other contributors.

The rest of this document is concerned with any changes impacting the `nickel`
repository, which may or may not involve changing the source of Nickel.

## Table of content

1. [Preamble](#preamble)
1. [Resources](#resources)
1. [Set up a development environment](#set-up-a-development-environment)
1. [How to submit changes](#how-to-submit-changes)

## Preamble

Before contributing any non-trivial change to this repository, please first
check for the existence of related work such as issues or open pull requests. If
there are none, it's better to discuss the change you wish to make via an issue,
by email, or using any other method with the maintainers of this repository
(listed below) before actually submitting something.

## Resources

### Documentation

The following resources are oriented toward Nickel users:

- The [README](./README.md) and the [design rationale](./RATIONALE.md)
- The [blog post series][blog-series] and the [release blog post][blog-release].
- The [user manual][user-manual].

For Nickel contributors (or aspiring contributors), the following technical
documentation is relevant as well:

- The [crate documentation][doc-crate].
- The [RFCs][rfcs]. There is currently no well established process for RFCs, but
  as a rule of thumb, impactful changes to the design or the implementation of
  the language are technically discussed and documented in a dedicated RFC
  document.
- The [technical notes][doc-notes]. Various notes gathering thoughts, proposals,
  or issues about an aspect of the language at one point in time that the author
  thought important to keep for posterity. They are usually more informal than
  RFCs, of smaller scope, and their content may become obsolete more easily.

### People

Nickel is maintained by [Tweag][tweag]. The current lead maintainer is Yann
Hamdaoui (@yannham).

You can find some of us on our [discord channel][nickel-chat] (and in
particular the Devs room), or fire an email at `nickel-lang@tweag.io`.

## Set up a development environment

Please refer to [`HACKING.md`](./HACKING.md) to set up a development environment
for Nickel, as well as adding or running tests and benchmarks.

## How to submit changes

**Try to keep pull requests small and focused**. Avoid packing refactoring,
cosmetic changes or anything not directly related to your original goal in the
same pull request. If preliminary steps make sense as standalone changes, don't
hesitate to split your pull request into several ones. There are a couple
aspects to consider when working on Nickel:

1. **Documentation**: If you added new items (functions, modules) to the public API
   of a crate, please document those items in-code. If you made a user-facing
   change (syntax, stdlib, language features, etc.), please update the existing
   documentation (in particular the user-manual) in consequence.
2. **Tests**: be it for bug fixing or adding new features, please try to write
   extensive tests as much as possible, to ensure the correctness of your
   changes as well as avoiding regressions.
3. **Benchmarks**: if your change is likely to impact the performance in a
   non-trivial way, it might be useful to run the benchmark suite on both master
   and on your branch and to report those results in the description of the PR.

## The review process, or how to get your PR merged

The conditions to merge a PR are whatever GitHub branch protection rules are
currently set by the Nickel repository. If a PR has the green button available
on GitHub, then this PR meets the conditions to be merged. Conditions usually
include at least one review from a maintainer and a green CI.

### As an external contributor

As an external contributor, the Nickel team is responsible for giving your code
a first review. Once done, they might require changes or ask questions. From
there, please try to address every review comment (and resolve the corresponding
conversation on GitHub), and request a new review once done.

### As a maintainer

If you are reviewing an external contributor's PR, _you are responsible for
merging or rejecting the PR_. In general, you should only give an "Accept" if
everything has been sorted out, and you're ready to click the green button
without needing another back-and-forth with the author.

If the author of a PR has commit rights (such as a Tweager or a Nickel
maintainer), _they are responsible for merging their change_. This includes
requiring review from the right people. When reviewing such PRs, you can leave
an "Accept" although you made review comments: this indicates that you are fine
with the work being merged as it is, but you have minor suggestions and
questions that the author might want to consider. You trust the author to do
whatever they think is best and proceed with merging the PR from there. This
minimizes back-and-forths with trusted authors.

[blog-series]: https://www.tweag.io/blog/2020-10-22-nickel-open-sourcing/
[blog-release]: https://www.tweag.io/blog/2022-03-11-nickel-first-release/
[user-manual]: https://nickel-lang.org/user-manual/introduction/
[doc-crate]: https://docs.rs/nickel-lang/0.1.0/nickel_lang/
[doc-notes]: notes/
[tweag]: https://www.tweag.io
[rfcs]: ./rfcs/
[nickel-chat]: https://discord.gg/vYDnJYBmax
[discord]: https://discord.gg/vYDnJYBmax
