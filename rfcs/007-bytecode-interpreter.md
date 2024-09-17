---
feature: bytecode interpreter
start-date: 2024-09-17
author: Yann Hamdaoui
---

# Bytecode interpreter

## Motivation

Performance hasn't been the initial focus of Nickel, because figuring out the
core language, tooling and developer experience was the first priority. The
language has now reach reasonable stability, and medium-size codebases show that
the performance can become a problem. More specifically, the memory usage and
the running time is far beyond not-so-optimized languages like Nix. We can do
better than the current situation.

There are many different ways to improve performance. One route is to apply
high-level optimizations to the source program, such as contract elision or
contract deduplication. Such optimizations have proven very effective, but the
most obvious ones are now implemented.

Another route that we will explore is caching, or incremental evaluation. This
is orthogonal and a possible game-changer if done right.

A third route is to optimize the interpreter itself. This includes general
program optimizations (that don't have much to do with the fact that Nickel is
the interpreter of a programming language), and more specific optimizations.

### Memory representation

It was noticed recently that `term::Term`, the main data structure representing a
Nickel value, has a whopping size of 464 bytes (!). While memory consumption is
not necessarily the main parameter to optimize for (our first target is running
time), such a size has the potential to stress the allocator (and dropping
code) and thus to affect the running time, in addition to making Nickel very
memory hungry.

Indeed, [#2022](https://github.com/tweag/nickel/pull/2022) shows that reducing
it to 74 bytes, which is still huge, can lead to 25-30% speedups on medium to
large codebases.

Similarly, `eval::stack::Marker` - which is what is stored on the evaluation
stack - is XXX bytes. Simply sprinkling `Box` here and there on both structures
might be able to achieve a combined gain of the order o f magnitude of up to a
50%, at the cost of making the implementation a bit uglier (one big issue of
`Box` is the inability to do pattern matching on it directly, which makes in
particular nested patterns impossible).

Nickel has been using, up to now, a unique representation of expressions from
parsing to evaluation. This is has the advantage of simplicity: there is only
one data type which is the single, shared source of truth. But this is at the
cost of specialization: this type gathers many different concerns, and several
constructors are useless at each stage of the pipeline. Analysis stages
(typically the LSP and the typechecker) are hurt by the need to handle runtime
representations (e.g. `Closure`), and by the fact that the parser needs to
already apply some desugaring to fit the unique AST structure, losing some
information on the way. Similarly, the evaluation stage needs to ignore or
evaluate away many things that are only relevant before program transformations.
Finally, uniqueness makes the representation big, as noted above.

### Intermediate representations

The usual way to address this issue is to adopt several representations, better
tailored to the needs of each stage. The earlier stages usually resort to a
usual tree-like AST representation, which is more adapted to program analysis
and transformations. The evaluation stage, on the other hand, is optimized for
performance. In particular:

- the general structure should be cache-friendly, that is usually array-like
    rather than tree-like (e.g. _flat ASTs_)
- the size of the atoms of the structure (nodes, values, whatever they are
  called) should be as small as possible, to amplify the cache-friendliness and
  alleviate the pressure on the allocator

### Toward a bytecode interpreter

Those constraints naturally lead to a bytecode compiler and a virtual
machine that will interpret this bytecode. This approach is not free:

- it can be substantially more complex to design and to implement than a
    tree-walking interpreter
- for small configurations with little to no actual evaluation, the cost of
   compiling and running the bytecode might be higher than the tree-walking
   approach

It is important to note that there is a whole spectrum between the naive
tree-walking interpreter and a full-fledged, JITed bytecode interpreter. All in
all, the simple fact of having a different representation for run-time values is
already departing from the pure tree-walking interpreter. I don't think we need
to go the route of a full-fledged low-level bytecode compiler and interpreter,
but rather to find a trade-off in-between, where compilation is a relatively
simple and cheap program transformation, while still giving some room for a more
optimized runtime representation and a faster evaluation scheme.

## VM examples

This section gathers several examples or real world languages and their virtual
machine as a source of inspiration and comparison. Those examples are selected
from either functional languages (lazy or not) and simple dynamic language (thus
with similar challenges regarding the representation of records).

### Lua

### OCaml

### Haskell

### JavaScript

## Proposal
