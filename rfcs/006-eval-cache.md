---
feature: Incremental evaluation
start-date: 2022-12-09
author: Yann Hamdaoui, Daniele Palombi
---

# Incremental evaluation
The goal of this RFC is to enrich the nickel evaluation model with an incremental
(re)computation mechanism.

## Motivation

### User-facing motivations

This RFC is motivated by the following examples:
1. **Efficient re-evaluation of nickel programs after small changes**
  Let's imagine we have a fairly large configuration (~1k lines, for reference [this](https://github.com/input-output-hk/mantis-ops/tree/master/jobs)). Say that we want to change line 14 [here](https://github.com/input-output-hk/mantis-ops/blob/master/jobs/tasks/morpho.cue), currently a new evaluation would have to recompute the whole program from scratch. This is not desirable for such a small, non-disruptive change.
  With a suitable incremental (re)computation strategy only fields that depend directly on line 14 would get recomputed. That would improve performance in a sensible manner since non-trivial computations like template on line 41 need only get evaluated once for both runs.

2. **Efficient recursive record merging/overriding**
  Let's consider the following example:
  ```
  let big_recursive_record = {...} in
  let fst_value = big_recursive_record.foo in
  let snd_value = big_recursive_record.bar in
  let overriden = big_recursive_record & {baz | force = "new_value"} in
  ...
  ```
  Where `foo` and `bar` are recursive fields but don't depend directly on `baz` and their value doesn't change as a result of the merge. Because `foo` and `bar` are recursive, in the current implementation, their value will be recomputed two times: one for `xxx_value` and one for `overriden`, although they are the same. An incremental version of the merging operation would avoid recomputing `foo` and `bar` in this case.

3. **Efficient re-evaluation of "similar" commands in the REPL**
  This is pretty much self-explainatory. See the previous two points.

TODO: Nix examples for 1 and/or 2?

### Technical motivations

1. **A cleaner revertible thunk-free implementation**

TODO: Talk about garbage collection.