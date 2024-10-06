---
feature: bytecode interpreter
start-date: 2024-09-17
author: Yann Hamdaoui
---

# Bytecode interpreter

## Motivation

Performance hasn't been the initial focus of Nickel. Figuring out the core
language, tooling and ensuring a minimal viable developer experience has been
the priority first. Now that the language has reached some stability, it becomes
apparent that the performance of the interpreter on medium-sized codebases is
suboptimal: it's both suboptimal in term of pure developer experience, and
compared to what an optimized interpreter written in Rust should be capable of.

The memory usage and the running time is far beyond not-so-optimized languages
like Nix. We can do better than the current situation.

There are many different routes to improve performance. One is to apply
high-level optimizations to the source program, such as static contract elision
or runtime contract deduplication. Such optimizations have proven very
effective, but the most obvious ones are now implemented.

Another route that we will explore is caching, or incremental evaluation. This
is orthogonal and a possible game-changer if done right. We'll make sure that
this proposal remains compatible with a future incremental evaluation
implementation.

A third route is to optimize the interpreter itself. This includes general
program optimizations (that don't have much to do with the fact that Nickel is
the interpreter of a programming language, such as using better algorithms and
data structures), and more interpreter-specific optimizations.

### Memory representation

It was noticed recently that `term::Term`, the main data structure representing
a Nickel value, has a whopping size of 464 bytes (!). While memory consumption
is not necessarily the main parameter we want to optimize for (our first target
is running time), such a size has the potential to stress the allocator and
dropping code, affecting the running time in addition to making Nickel very
memory hungry.

Indeed, [#2022](https://github.com/tweag/nickel/pull/2022) shows that reducing
it to 74 bytes, which is still very big compared to other language runtimes, can
lead to 25-30% speedups on medium to large codebases.

`eval::stack::Marker` - which is what is stored on the evaluation stack - is in
the same ballpark. Simply sprinkling `Box` here and there on both structures
might be able to achieve a combined gain up to rougly 50%, at the cost of making
the implementation a bit uglier (one big issue of `Box` is the inability to do
pattern matching on it directly, which makes in particular nested patterns
impossible).

Nickel has been using, up to now, a unique representation for expressions from
parsing to evaluation. This is has the advantage of simplicity: there is only
one data type which is the single, shared source of truth. But this is at the
cost of specialization: this structure gathers many different concerns, and several
constructors are useless at each stage of the pipeline. Analysis stages
(typically the LSP and the typechecker) are hurt by the need to handle runtime
representations (e.g. `Closure`), and by the fact that the parser needs to
already apply some desugaring to fit the unique AST structure, losing some
information on the way. Similarly, the evaluation stage needs to ignore or
evaluate away many things that are only relevant before program transformations.
Finally, uniqueness makes the representation big, as noted above.

### Intermediate representations

The usual way to address this issue in a compiler is to adopt several
representations, better tailored to the needs of each stage. The earlier stages
usually resort to a usual tree-like AST representation, which is more adapted to
program analysis and transformations. The evaluation stage, on the other hand,
is optimized for performance. In particular:

- the general structure should be cache-friendly, that is usually array-like
    rather than tree-like (e.g. flat ASTs or compact bytecode)
- the size of the atoms of the structure (instructions, values) should be as
  small as possible, to amplify the cache-friendliness and alleviate the
  pressure on the allocator. Of course, there's some trade-off here, as reducing
  the size is often done by adding indirections, but this also has a cost at
  runtime.

### Toward a bytecode interpreter

Those constraints naturally lead to a bytecode compiler and a virtual
machine that will interpret this bytecode. This approach is not free:

- it can be substantially more complex to design and to implement than a
    tree-walking interpreter
- for small configurations with little to no actual evaluation, the cost of
   compiling and running the bytecode might be higher than the tree-walking
   approach

It is important to note that there is a whole spectrum between the naive
tree-walking interpreter and a full-fledged, low-level, JITed bytecode
interpreter. Thee simple fact of having a slightly different representation for
runtime expressions is already departing from the pure tree-walking interpreter.
As we might have to perform compilation on-the-fly, we should find a trade-off
in-between, where compilation is a relatively simple and cheap program
transformation while still giving some room for a more optimized runtime
representation and thus faster evaluation.

## VM examples

This section gathers examples of the virtual machines of other interpreted
languages as a source of inspiration and comparison. Those examples have been
selected to include a variety of designs and constraints, from either strict or
non-strict functional languages, statically typed or dynamically typed (thus
with similar challenges regarding the representation of records).

We'll look more specifically into the following aspects:

- the memory representation of values and in particular closures and records.
  By closures, we mean functions or unevaluated expressions (thunks) that might
  capture part of their outer environment. Because Nickel is lazy, basically
  most expression are potentially closures
- the general architecture of the virtual machine (stack-based, register-based,
    representation of the environment, etc.)
- high-level (more instructions, more complex VM, simpler compilation scheme) vs
    low-level

### Lua

Lua is an efficient scripting language. The design of the virtual machine is
intended to provide a lightweight and portable VM where code is both simple and
fast to compile, and to run. Indeed Lua is often embedded in other software
projects (games, industrial systems, etc.).

The Lua virtual machine has some optimizations that are quite Lua-specific. Lua
represents everything as dictionaries, including traditional contiguous arrays -
they must optimize such encoding to get good performance, which is rather
irrelevant for Nickel. It is nonetheless an interesting inspiration as a simple
runtime of a dynamically typed language where dictionaries are ubiquitous (kinda
like in Nickel) and with function closures.

#### Memory representation

Lua has 8 primitive types: `nil`, `boolean`, `number`, `string`, `function`,
`table`, `userdata` and `thread`. Values are represented as a tagged union.
`nil`, `boolean` and `number` are represented inline (unboxed). `string`,
`function`, `table` and `userdata` are represented as pointers to the actual
data.

This representation takes between 3 and 4 words per value (depending on the
architecture), which isn't small by VM standards. Smalltalk, for example, uses
spare bits in each pointer to reduce the number of words used, but this isn't
portable or implementable in pure ANSI C.

Because tables are used to represent arrays, Lua 5 uses a hybrid representation
with an array part and a hash part.

Lua also has a peculiar representation of closures. There is a vast body of
literature around compilation of closures to native code in functional
languages, but the analyses involved can be expensive or too complex for a
simple one-pass compiler. Instead, in Lua, outer variables are accessed
indirectly through a slot called an _upvalue_, which originally points to the
stack of the enclosing function owning the referenced value. When the stack
frame is freed, the value is moved to the slot so that it can outlive the
enclosing function call. Thanks to the additional indirection, this move is
invisible to the closures themselves. This is combined with flat closures
(access to more than one outer level causes the corresponding variable to be
pulled in the enclosing function), and a list of open upvalues to ensure that
there's only on such slot pointing to one given value at all time, even if this
slot is shared by many closures.

#### Virtual machine

The Lua virtual machine is register-based since Lua 5.0 (note that there's still
a stack for activation records) while most bytecode VMs out there are usually
stack-based, even if they might have a few registers. Lua developers argue that
a register-based VM saves many `push` and `pop` operations, which both avoids a
non-trivial amount of copying (especially with the tagged union representation)
and reduces the number of instructions (albeit the instructions are larger,
because they often need additional operands to indicate which registers they
operate on). Registers also reduce the number of instructions needed: the Lua VM
only has 35. The Lua VM has 256 registers represented as an array at runtime,
guaranteeing fast access.

#### References

- [The Implementation of LUA 5](https://www.lua.org/doc/jucs05.pdf)

### OCaml

#### Memory representation

OCaml uses a uniform memory representation where any value is represented as a
single machine word. Unboxed values (integers, floats, etc.) are distinguished
from pointers by their least significant bit (and are thus encoded on `n-1` bits
compared to their, say, C equivalent). This is needed for garbage collection
only.

Boxed values are represented as a pointer to a block, which is a contiguous area
of the memory with a one-word header. The header holds:

- the size
- the color (for garbage-collection)
- a multi-purpose tag byte

The header followed by some arbitrary content, whose shape depends on the type
of the value.

ADTs are represented within the block's data as an integer if there are no
parameters (the tag byte then doesn't store the actual variant's tag, but has
the same value than for an `int`). For a variant with parameters, the tag byte
is used to encode the variant and the argument is stored as a word of block
content.

Tuples, records and arrays are stored as a contiguous C-style array of values.

A single closure[^mut-rec-block] is in general represented using 3 words

- the function pointer
- the arity of the function and the offset for the environment of the closure
  packed in one word
- a function pointer for full application (an optimization used to avoid
  creating useless partial applications when the function is fully applied).

[^mut-rec-block]: OCaml also represents a block of mutually recursive functions
    together, which is mostly a sequence of closures with a common environment
    and an infix header in-between

#### Virtual machine

The OCaml virtual machine is based on the ZAM (ZINC Abstract Machine) experiment
by Leroy, although it has evolved a bit since then (now ZAM2). The ZAM is a
stack-based virtual machine derived from standard machines for the call-by-value
lambda-calculus (like the SECD machine). The particularity of the ZAM is to be
optimized for currying and partial application of closures: it uses a
"push-enter" evaluation strategy, as in the Krivine machine, where the SECD uses
"eval-apply". The environment has a representation close to the current
environment representation in Nix, that is a linked list of arrays where a new
array is appended for each new scope (when there's a sharing point).

The ZAM has 145 instructions where many of them are variations of the same
operation (`APPLY`, `APPLY1`, `APPLY2` and `APPLY3` for instance). They are
reduced to rather low-level operation: environment manipulation, function
application, boolean and integers operations, branching, exceptions, memory
allocation/value creation.

While the ZAM is considered stack-based, it has a few predefined registers:

- PC: program counter
- SP: stack pointer
- ACCU: accumulator
- TRAPSP: stack pointer of highest exception handler
- EXTRA_ARGS: number of extra arguments to function application
- ENV: environment
- GLOBAL: global data

#### References

- [Real World OCaml chapter on bytecode](https://dev.realworldocaml.org/compiler-backend.html),
- [Original Leroy's paper: _The ZINC experiment: an economical implementation of
  the ML language_ (1990)](https://inria.hal.science/inria-00070049/document)
- [OCaml bytecode instructions](http://cadmium.x9c.fr/distrib/caml-instructions.pdf)
- [OCaml memory representation of values](https://ocaml.org/docs/memory-representation)
- [From Krivine's machine to the Caml implementations](https://xavierleroy.org/talks/zam-kazam05.pdf)
- [OCaml's representation of closures](https://github.com/Gbury/ocaml-memgraph/blob/master/doc/closures.md)

### Haskell

Despite not being advertised, Haskell has an interpreter as well, which is used
mostly for the GHCi REPL. This section describes what we know of the actual the
bytecode interpreter, but also incorporates some aspects of the original STG
machine which is the basis of the low-level operational semantics of Haskell.

#### Memory representation

Closures (thunks and functions) are represented uniformly as an environment and
a code pointer. In some other machines for lazy lambda calculi, thunks hold
additional data such as a flag (evaluated or suspended) and a potential value
that is filled after the thunk has been forced. When accessing a thunk, the
caller checks the state, and then either retrieve the value or initiate an
update sequence and retrieve the code of the unevaluated expression. In Haskell,
the update process performed by the callee instead (the thunk's code), such that
thunk access is uniform: just jump to the corresponding code. As with other VMs,
the environment - immediately inlined after the code pointer - stores the free
variables of the expression that are captured by the closure. When entering a
closure, a dedicated register holds the pointer to the environment for fast
access.

The STG paper argues that this simplifies the compilation process and gives room
for some specific optimizations (vectored return for pattern matching, for
example) that should be beneficial to Haskell programs.

In Haskell, every data is considered to be an algebraic data type including
primitive types such as integers, which is just `data Int = MkInt Int#`  where
`Int#` is a native machine integer. The rationale is to avoid having types being
blessed by the compiler with some magic while ADTs, which are ubiquitous in
functional programming, are second-class citizen (with respect to optimizations
and performance). In consequence, ADTs are the foundation of data structures.

Both data and closures are represented the same way. A data value is a block
with a code pointer, which corresponds to the corresponding ADT constructor.
Instead of the environment in the case of closures, the code pointer is followed
by the constructor's argument. As each constructor usage potentially generates
very similar code, GHC is smart enough to share common constructors instead of
generating it again and again. Specific optimizations or compilation schemes are
implemented to alleviate the cost of the (code) pointer indirection for data
values.

In practice, when compiled to native code, the closures

#### Virtual machine

The STG machine has five components:

- the code
- the argument stack which contains values. Values are either a heap address
    `Addr a` or a primitive  integer value `Int n`.
- the return stack which contains continuations
- the update stack which contains update frames
- the heap storing (only) closures
- the global environment which gives the addresses of closures defined at the top
    level

When compiling to native machine code, the three stacks (argument, return and
update) are merged into two stacks - but it's only for garbage collection
reasons: there is a pointer stack, and a value stack, as the garbage collector
couldn't tell the difference otherwise. If possible,it's simpler and usually
better to just merge all the stacks into one, as long as they grow
synchronously.

The environment, which is just an abstract map data structure in the STG
operational semantics is split in the actual implementation between the stack
(arguments), the environment register (captured variables or _upvalues_) and the
heap register. Local variables introduced by let-bindings are allocated by
bumping the heap register and are accessible directly from there: the heap is
indeed just a bump-allocator than triggers copying garbage collection when full.
In particular, local let-bound variables aren't pushed to the stack unless there
is a context switch caused by eager evaluation, that is a case expression
forcing an argument. In this case, each live variable not on the stack (id est
let-bound variables and captured variables) are stored on the stack prior to the
context switch.

#### References

- [The STG machine](https://dl.acm.org/doi/pdf/10.1145/99370.99385)
- [Bytecode instruction datatype definition in GHC](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/ByteCode/Instr.hs#L60)

### JavaScript (V8)

JavaScript doesn't have one official virtual machine, but rather many different
performance-oriented implementations. We'll focus on the V8 engine, which is one
of the fastest, ubiquitous and most used JavaScript engine. V8 is a really
complex JIT compiler, which doesn't really fit the model of a basic bytecode
interpreter. Still, V8 features the Ignition bytecode interpreter as part of its
pipeline and implements noteworthy memory representation optimizations.

#### Memory representation

V8 uses a technique called _fast properties_ to represent objects. One
ingredient is in not very different from the technique used in Lua: have an
array part for contiguous properties and a dictionary part for the rest. We
won't expand on the handling of the array part, which is currently useless for
Nickel, having a first-class arrays.

Closures are represented together with their environment of captured variables,
as in most other VMs. However, this environment is created eagerly in V8 when
the parent function of the closure is created (this is called a _context_). It
also seems that the whole local environment of the outer scope is kept around
and shared by all closures contained in a scope, instead of each closure pulling
and copying what it needs from the outer environment. The performance profile is
thus a bit different than, say, OCaml: V8 might retain more memory alive, in
particular with nested callbacks, but it can also save some copying of captured
variables.

V8 keeps the pointer to the closure's environment in a dedicated register (rsx)
to access such variables quickly.

The dictionary part isn't necessarily represented as a proper hashtable, because
this doesn't play well with [inline
caching](https://en.wikipedia.org/wiki/Inline_caching), so V8 tries to avoid
them. By default, initial properties are inlined as an array in the object's
data, and a separate metadata structure - the `HiddenClass` - is used to map
names to offsets. Accessing inline properties is the fastest, but adding and
removing them requires to modify both the object and the `HiddenClass`. At some
point it can become too costly to keep doing that, so V8 will switch to a
standard self-contained key-value map at some point (_slow properties_).

#### Virtual machine

V8 is designed to be an optimizing JIT native code compiler. Ignition adds a new
intermediate representation, higher-level than native code but lower-level than
the AST, that serves as a compact runtime representation that is cheap to
compile to. The bytecode is designed to be easily specialized to native code
when needed. Finally, the bytecode serves as a single exchange format the
different optimizing compilers.

Ignition's bytecode has been optimized for size more than for speed, the main
motivation being to reduce the memory footprint of the V8 engine (in practice,
it can be a problem on mobile device). Ignition is register-based, with an
unbounded number of local registers (or so it seems, which are stored on the
local stack of the function - so register are more like stack slots?). The
Ignition interpreter is more like a very lazy compiler: interpretation is done
by generating macro-assembly, some kind of portable assembly used by the
Turbofan optimizing compiler, that is then transpiled by Turbofan to specialized
machine code. This is done on-the-fly for each bytecode instruction.

#### References

- [V8 documentation: fast properties](https://v8.dev/blog/ignition-interpreter)
- [V8 documentation: Ignition interpreter](https://v8.dev/blog/ignition-interpreter)
- [V8 implementation: list of instructions](https://github.com/v8/v8/blob/master/src/interpreter/bytecodes.h)
- [Grokking V8 closures for fun and profit](https://mrale.ph/blog/2012/09/23/grokking-v8-closures-for-fun.html)

### Tvix

Tvix is a recent re-implementation from scratch of an evaluator for the Nix
language. Evaluation of large Nix expressions is notoriously slow, and has a
direct impact on the user experience - even when the user isn't a Nix developer
but just someone using Nix to install packages.

We picked Tvix because the Nix language shares many performance characteristics
a challenge with Nickel: it is a dynmaically typed, (almost) pure functional
language that makes extensive use of records and fixpoints.

#### Memory representation

Attribute sets are represented in Tvix either as the special value `Empty`, a
dedicated inline representation for attribute sets of size 2 of the form `{ name
= .., value = ..}`, or a `BTreeMap<NixString, Value>`.

Closures (either thunks or functions) are represented mostly as a code pointer
and an array of upvalues (variables captured from the upper environment). This
is quite similar to the Haskell and OCaml runtime representations.

#### Virtual machine

The Tvix virtual machine is a stack-based bytecode interpreter. The
[instructions](https://docs.tvix.dev/rust/tvix_eval/opcode/enum.Op.html) are
slightly higher-level than the other examples, with several specific
instructions around the creation and manipulation of attribute sets, thunks and
functions.

The code consist of an instruction segment (`code`), a value segment
(`constant`), and a span segment. The code is an array of operations each
encoded on one byte. Some instructions actually have operands, that are decoded
by the VM when needed.

Although Nix is a lazy language, Tvix departs from the push-enter model
implemented in OCaml's ZAM and Haskell's STG by having a proper `call`
instruction. Compiling an expression in Tvix results in leaving some result on
the stack. When evaluating an application `f x`, code is generated for `x`,
followed by `f`, and finally a `CALL` opcode is generated. On paper at least,
this looks less adapted for partial application and currying, at least without
further optimizations. On the other hand, this plays well with the way Tvix
represents local variables, which are directly stored on the stack (of course,
the thunks bound by the variables are stored on the heap but their address
remains on the stack).

Looking at the implementation, it also seems that local stack slots are only
deallocated after the whole code of the function has ben run. But in a lazy
language a variable in head position is basically a tail-call, so it looks like
Tvix is retaining much more memory than necessary: for example, in `let f = x:
_continuation_; in let a = 1 + 1; in let b = a + 2; in f b`, `a` and `b` looks
like they would remain alive on the stack for the whole duration of
`_continuation_` while they aren't accessible anymore. But I might be missing
something as well.

#### References

- [Tvix source code](https://github.com/tvlfyi/tvix)

## Proposal

- Closure representation: although this looks simplistic, Peyton-Jones argues in
    STG-machine that copying the upvalues into the closure is actually one of
    the most efficient in practice (smarter strategies don't gain much time and
    can cost a lot of memory). So we should probably just do basic closure
    conversion. Not sure the Lua approach is really useful as a first shot,
    because as I understand it, you need to produce the upvalue move instruction
    when a scope ends.
- Record representation: it's honestly a bit irrelevant, because it can evolve
    independently from the bytecode instructions and other representations.
    Maybe we should use persistent data structure? Still, there might be some
    specific stuff related to the recursive environment and the "lazyness" of
    the recursive environment. Maybe compile that in the same way as OCaml
    mutually recursive closures.
- Array repr: todo. RPDS?
- Stack elements repr (equality, deep_sequing, argument tracking and other
    specifities of Nickel).
- Number of stacks? Registers? Probably one stack. Should we have a bunch of
    registers as in Lua?
- Instruction set: the TVIX one looks like the more adapted to our case.
- Scope management: stack-allocate let-bindings locally, as in Tvix, but we need
    to clean them. Most probably, a cleaning is mandated when entering a thunk,
    as the environment is 

## Optimizations

- Haskell's update flag: avoid creating a thunk in the first place when in
  normal form or when we can prove that the thunk is only used once
- multi-ary merging (in `a & b & c`, instead of evaluating `r1 <- a & b`, then
    `r2 <- c`, then `r1 & r2`, use some `merge_n` primitive).
