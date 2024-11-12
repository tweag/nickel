---
feature: bytecode interpreter
start-date: 2024-09-17
author: Yann Hamdaoui
---

# RFC007: Bytecode interpreter

## Motivation

The first priorities of the Nickel project have been figuring out the core
language, implementing basic tooling and ensuring a minimal viable developer
experience. Now that the language has reached some stability, it becomes
apparent that the performance of the interpreter beyond small-sized codebases is
suboptimal: in absolute term regarding developer experience, compared to some
other configuration languages (such as Nix, albeit not being especially known as
very performant), and compared to what an optimized interpreter written in Rust
should be capable of. We can do better than the current situation.

There are many different routes to improve performance. One is to apply
high-level optimizations to the source program, such as static contract elision
or runtime contract deduplication. Such optimizations have proven very
effective, but as we implement them, there are less and less potentially
impactful ones left.

A second route is to optimize the interpreter itself. This includes general
program optimizations (that don't have much to do with the fact that Nickel is
the interpreter of a programming language, such as using better algorithms and
data structures) and more interpreter-specific optimizations. The current RFC
focuses on this route.

A third route that we will explore in a future RFC is caching, or incremental
evaluation. This is orthogonal and a possible game-changer if done right. We'll
make sure that the current proposal wouldn't hurt the fundamental principles of
incremental evaluation.

### Memory representation

It was noticed recently that `term::Term`, the main data structure representing
a Nickel value, has a whopping size of 464 bytes (!). While memory consumption
isn't necessarily the main parameter we want to optimize for (our first target
is running time), such a size has the potential to stress the allocator and
dropping code, as well as filling the cache, affecting the running time in
addition to making Nickel very memory hungry.

Indeed, [#2022](https://github.com/tweag/nickel/pull/2022) shows that reducing
it to 74 bytes, which is still huge compared to other language runtimes, can
lead to 25-30% speedups on medium to large codebases.

`eval::stack::Marker` - which is what is stored on the evaluation stack - is in
the same ballpark. Simply sprinkling `Box` here and there on both structures
might be able to achieve a combined gain up to rougly 50%, at the cost of making
the implementation a bit uglier (one big issue of `Box` is the inability to do
pattern matching on it directly, which makes in particular nested patterns
impossible).

### Intermediate representations

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
  the size is often done by adding indirections, but this also has a cost.

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
interpreter. The simple fact of having a slightly different representation for
runtime expressions is already departing from the pure tree-walking interpreter.
As we might have to perform compilation on-the-fly, we should find a balance,
where compilation is a relatively simple and cheap operation while still giving
some room for a more optimized runtime representation and thus faster
evaluation.

For example, we might not want to have as many intermediate representations as
an ahead-of-time native code compiler because we pay the price of lowering
(transforming from one representation to the next) at each execution. Still, the
optimum is most likely to have at least two representations, with one optimized
for runtime.

## Virtual machines in the wild

This section gathers examples of the virtual machines of other interpreted (and
sometimes compiled) languages as a source of inspiration and comparison. Those
examples have been selected to include a variety of design choices and
constraints, from either strict or non-strict functional languages, statically
typed or dynamically typed.

We'll look more specifically into the following aspects:

- the memory representation of values and in particular closures and records.
  By closures, we mean functions or unevaluated expressions (thunks) that might
  capture part of their outer environment. Because Nickel is lazy, most
  expressions are potentially closures.
- the general architecture of the virtual machine (stack-based, register-based,
  high-level vs low-level, etc.)
- the representation of the environment (local variables, captured variables,
    global variables)

### Lua

Lua is an efficient scripting language. The design of the virtual machine is
intended to provide a lightweight and portable VM where code is both simple and
fast to compile, and to run. Indeed Lua is often embedded in other software
projects (games, industrial systems, etc.) where it must be responsive.

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
portable or implementable in pure ANSI C. For reference, OCaml uses only one
word per value (of course, it's an indirection to actual value _data_, which
might be way bigger) thanks to pointer tagging and `wordsize-1`-bits integers.

Because tables are used to represent arrays, Lua 5 uses a hybrid representation
with an array part and a hash part. Once again, this is irrelevant for Nickel.

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

Compared to the traditional closure conversion of Haskell, it seems that the
advantage is to avoid copying as much as possible (given Lua's "large" value
representation). First, many closures can share the same slot, instead of each
having their own copy of the captured value. Second, I suppose that if the
enclosing function outlives the closure, no value is ever moved out from the
slot, and thus no copying happens.

On the other hand, this techniques looks useless if the values are represented
on one word, as a pointer to the slot and the slot itself still need to be
allocated, plus the machinery around that (list of open closures during
compilation and at runtime to decide what should be moved when de-allocating a
frame).

#### Virtual machine

The Lua virtual machine is register-based since Lua 5.0 (note that there's still
a stack for activation records) while most bytecode VMs out there are usually
stack-based, even if they might have a few registers. Lua developers argue that
a register-based VM saves many `push` and `pop` operations, which both avoids a
non-trivial amount of copying (especially with the "large" tagged union
representation of values) and reduces the number of instructions (albeit the
instructions are larger, because they often need additional operands to indicate
which registers they operate on). Registers also reduce the number of
instructions needed: the Lua VM only has 35. The Lua VM has 256 registers
represented as a packed array at runtime, guaranteeing fast access.

#### References

- [The Implementation of LUA 5](https://www.lua.org/doc/jucs05.pdf)

### OCaml

#### Memory representation

The following notes on the memory representation applies to the native code
backend's representation. As far as I can tell, closures are represented the
same way in the Zinc Abstract Machine. Itmust be so at least to some degree
forthe sake of the FFI where OCaml values can be manipulated.

OCaml uses a uniform memory representation where any value is represented as a
single machine word. Unboxed values (integers, etc.) are distinguished from
pointers by their least significant bit (and are thus encoded on `n-1` bits
compared to their, say, C equivalent). This is needed for garbage collection
only.

Boxed values are represented as a pointer to a block, which is a contiguous area
of the memory with a one-word header:

The header holds:

- the size
- the color (for garbage-collection)
- a multi-purpose tag byte

The header is followed by arbitrary content whose shape depends on the type of
the value.

```text
| header (1w)        | content   |
----------------------------------
| size | color | tag | data ...  |
```

ADTs are represented as an unboxed integer for constructors with no argument.
For a variant with parameters, the tag byte is used to encode the variant and
the argument is stored as a word of block content.

Tuples, records and arrays are stored as a contiguous array of data.

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
"push-enter" evaluation strategy, as in the Krivine machine, where the
traditional call-by-value machines such as the SECD uses "eval-apply".

Arguments and local variables are stored on the stack (the volatile part of the
environment), and other captured variables are saved on the heap together with
closures.

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
a pointer to a closure info table. The closure info table contains the code
pointer and other garbage-collection related data. The closure info table is
generated only once per binding appearing in the code and thus shared by dynamic
instances of the same code. In some other machines for lazy lambda calculi,
thunks hold additional data such as a flag (evaluated or suspended) and a
potential value that is filled after the thunk has been forced. When accessing a
thunk, the caller checks the state, and then either retrieve the value or
initiate an update sequence and retrieve the code of the unevaluated expression.
In Haskell, the update process is performed _by the callee_ instead (the thunk's
code), such that thunk access is uniform: it's an unconditional jump to the
corresponding code.

While on paper it seems like there are two indirections (pointer to the closure
info table -> code pointer -> code), a pointer to the info table is actually a
direct pointer to the code while the rest of the metadata lies _before_ it, at
least for native code generation. Entering the code is then a single indirect
jump instruction on most CPUs, and metadata can be accessed by subtracting from
the code pointer.

As with other VMs, the environment -- immediately inlined after the code pointer
-- stores the free variables of the expression that are captured by the closure.
When entering a closure, a dedicated register holds the pointer to the
environment for fast access.

The STG paper argues that this uniform thunk representation (with "self-handled
update") simplifies the compilation process and gives room for some specific
optimizations (vectored return for pattern matching, for example) that should be
beneficial to Haskell programs.

In Haskell, every data is considered to be an algebraic data type, including
primitive types such as integers, which is just `data Int = MkInt Int#` (where
`Int#` is a native machine integer). Indeed, ADTs are ubiquitous in functional
programming, and should be first-class citizen (regarding performance in
particular). In consequence, ADTs are the basis of all data values. The
primitive operation to look at them is the `case` expression (this is a
low-level case, more primitive than the fancy pattern matching of the surface
language), which forces its argument.

Both data and closures are laid out in a similar way in memory. A data value is
a block with a code pointer representing the corresponding ADT constructor. The
code pointer is followed by the constructor's argument (instead of the
environment in the case of closures). As each constructor usage potentially
generates very similar code, GHC is smart enough to share common constructors
instead of generating them again and again (typically the one for an empty
list).

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
reasons: there is a pointer stack and a value stack, as the garbage collector
couldn't tell the difference otherwise. If possible, it's usually better to
merge all the stacks into one, as long as they grow synchronously (for data
locality and simplicity).

The environment, which is just an abstract map data structure in the STG
operational semantics, is split in the actual implementation between the stack,
the environment register and the heap register.

Function arguments are pushed on the stack and accessed by a statically known
fixed offset.

Captured variables (Lua's _upvalues_) are stored within the closure and accessed
by a statically known offset from the special _environment register_, which is
set to point to the closure's data upon entry.

Local variables introduced by let-bindings are allocated by bumping the heap
register and are accessible from a known fixed register as well: the heap is
indeed just a bump-allocator which triggers copying garbage collection when
full. As long as there's  no garbage collection, it thus acts like another
stack, from which local variables can be retrieved from a known offset.

This means local let-bound variables aren't pushed to the stack unless there is
a context switch caused by eager evaluation, that is a `case` expression forcing
an argument. Before evaluating a case expression, each live variable that is not
on the stack (id est let-bound variables and captured variables) are pushed on
the stack prior to the context switch and restored after return.

#### References

- [The STG machine](https://dl.acm.org/doi/pdf/10.1145/99370.99385)
- [Bytecode instruction datatype definition in GHC](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/ByteCode/Instr.hs#L60)

### JavaScript (V8)

JavaScript doesn't have one official virtual machine, but rather many different
performance-oriented implementations. We'll focus on the V8 engine, which is one
of the fastest and most used JavaScript engine. V8 is a really complex JIT
compiler, which doesn't fit the model of a basic bytecode interpreter very well.
Still, V8 features the Ignition bytecode interpreter as part of its pipeline and
implements noteworthy memory representation optimizations.

#### Memory representation

V8 uses a technique called _fast properties_ to represent objects. One
ingredient is similar to Lua's array representation: have an array part for
contiguous properties and a dictionary part for the rest. Once again, we aren't
interested in the array part, since Nickel has separate first-class arrays.

Closures are represented together with their environment of captured variables,
as in most other VMs. However, this environment is created eagerly in V8 when
the parent function of the closure is entered (this is called a _context_). It
also seems that the whole local environment of the outer scope is kept around
and shared by all closures contained in a scope, instead of having each closure
pulling and copying what it needs from the outer environment.

V8 keeps the pointer to the closure's environment in a dedicated register (rsx)
to access such variables quickly, as in e.g. OCaml and Haskell.

The dictionary part isn't necessarily represented as a proper hashtable, because
this doesn't play well with [inline
caching](https://en.wikipedia.org/wiki/Inline_caching). Because inline caching
is so important for JavaScript's performance, V8 tries hard to avoid pure
dynamic dictionaries.

By default, initial properties are inlined as an array in the object's data, and
a separate metadata structure - the `HiddenClass` - is used to map names to
offsets. `HiddenClass`es are a sequence of transitions from the initial state of
the object (create empty object, insert property `a`, insert property `b`) and
determines the layout of the object and how to access inline properties. The
hypothesis behind inline caching is that the usage site of a property - say
`myparam.fooBar()` - often seems `myparam` values with the same `HiddenClass`,
such that the code can be specialized to fetch the property at a given offset.
If the `HiddenClass` happens to be different to the one of the cache, the code
falls back to a generic (and slow) access pattern.

However, the `HiddenClass` isn't free to maintain. Adding and removing
properties requires to modify both the object and the `HiddenClass`. At some
point it can become too costly to keep doing that, so V8 will switch to a more
standard self-contained key-value map at some point (_slow properties_), at the
cost of inline caching.

#### Virtual machine

V8 is designed to be an optimizing JIT native code compiler. Ignition adds a new
intermediate representation, higher-level than native code but lower-level than
the AST, that serves as a compact runtime representation that is cheap to
compile to. The bytecode is designed to be easily specialized to native code
when needed. Finally, the bytecode serves as a single exchange format for the
different optimizing compilers (TurboFan and Crankschaft, at least when the
latter still existed).

Ignition's bytecode has been optimized for size more than for speed, the main
motivation being to reduce the memory footprint of the V8 engine (memory
consumption is an issue on mobile devices and for startup time). Ignition is
register-based, with an unbounded number of local registers (or so it seems,
which are stored on the local stack of the function - so register are more like
stack slots?). The Ignition interpreter works as a _very lazy_ native code
compiler: interpretation is done by generating macro-assembly, a portable
template assembly used by the Turbofan optimizing compiler, that is then
transpiled by Turbofan to specialized machine code. This is done on-the-fly for
each instruction.

#### References

- [V8 documentation: fast properties](https://v8.dev/blog/ignition-interpreter)
- [V8 documentation: Ignition interpreter](https://v8.dev/blog/ignition-interpreter)
- [V8 implementation: list of instructions](https://github.com/v8/v8/blob/master/src/interpreter/bytecodes.h)
- [Grokking V8 closures for fun and profit](https://mrale.ph/blog/2012/09/23/grokking-v8-closures-for-fun.html)

### Tvix

Tvix is a recent re-implementation from scratch of an evaluator for the Nix
language. Evaluation of large Nix expressions is notoriously slow, and has a
direct impact on the user experience - even when the user isn't a Nix developer
but just someone using Nix to install packages. Tvix aims at making Nix
evaluation faster, among other things.

We picked Tvix because the Nix language shares many performance characteristics
with Nickel: it is a dynmaically typed, (almost) pure functional language that
makes extensive use of records and fixpoints.

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
followed by `f`, and finally a `CALL` opcode is generated. This is the
eval-apply model, which should be less adapted to currying and partial
application, at least without further optimizations.

Looking at the implementation, it also seems that local stack slots are only
deallocated after the whole code of the function has been run. In a lazy
language any closure entry is a tail-call, so it looks like Tvix is retaining
more memory than necessary[^tvix-no-documentation]: for example, in `let f = x:
_continuation_; in let a = 1 + 1; in let b = a + 2; in f b`, `a` and `b` looks
like they would remain alive on the stack for the whole duration of
`_continuation_` although they aren't accessible anymore

[^tvix-no-documentation]: Since the architecture of the Tvix VM isn't
    technically documented, this is based on my understanding of the codebase in
    a limited time; I could be missing something.

#### References

- [Tvix source code](https://github.com/tvlfyi/tvix)

## Proposal

The proposal takes inspiration from the previous VM descriptions, plus common
and folklore knowledge about VM and interpreters in general. The design is
guided by the following principles:

- **fast compilation**: although we might provide facilities to distribute
    compiled Nickel code in the future, this is just a bonus. The normal
    workflow we want to optimize for is to compile Nickel code from source and
    run it in a row. This must remain competitive with the current evaluation
    model for small configurations with little to no computation. Thus, the
    compiler must be fast (which often means it must be simple). Ideally, all
    the program transformations and the compilation should be doable in one pass
    (this might be hard to satisfy depending on current and future program
    transformations, but it's an ideal goal).
- **simplicity**: it goes hand in hand with fast compilation. But beyond
    performance, we want to lay the foundation for a maintainable virtual
    machine and thus start simple. We can leave many improvements and
    optimizations for the future.
- **optimize for speed**: in general, we favor optimizing for speed rather than
    memory consumption, even if because of memory allocation, both are
    correlated.

### Virtual machine architecture

The virtual machine is a stack-based machine with a set of working registers.

It follows the same general principles as the ZAM, the Tvix VM or the STG
machine. Those machines still have a number of differences on how they handle
fundamental operations such as function application. We take inspiration from
the
[call-by-push-value](https://www.cs.bham.ac.uk/~pbl/papers/thesisqmwphd.pdf)(CBPV)
as a guideline to handle both strict and non-strict evaluation in the same VM.

CBPV's computations correspond to a sequence of bytecode instructions. CBPV's
values are heap-allocated data. CPBV's `return`, `thunk` and `force` primitives
are implemented by eponymous instructions:

- `return` instruction is used to return a value from a computation and will in
    effect push this value on the stack.
- `thunk` turns a suspended computation to a value by allocating a closure, and
    pushes it on the stack
- `force` saves the current continuation, grabs the top of the stack, expects a
    closure, and enters it.

We follow the push-enter model which looks more adapted to a non-strict language
with currying by default. Functions are responsible for checking if they are
partially applied and create a partial application closure accordingly.

#### Machine state

The VM state is represented by a set of registers and a stack, which mixes
arguments, return addresses, update frame, local variables, and others.

The basic registers are:

- `next_instruction : CodeAddress`: the address of the next instruction to
    execute (program counter)
- `current_closure : Option<Closure>`: a pointer to the closure being evaluated,
    if any. The VM can offset from there to access the closure's environment.
    Corresponds to OCaml's `ENV` or Haskell's `Node`.

More registers might be added (such as one or several accumulators or working
registers) as the implementation progresses.

#### Environment and scopes

The environment is split between a global environment (with the stdlib loaded),
the stack (function arguments and local let bindings) and the closures'
environment for captured variables. A scope correspond to either a function or a
thunk and is introduced by:

- a function definition: `fun <arg> => <body>`
- argument to a function call: `f (let y = 1 in y)`
- the bound expression in a let-binding: `let <var> = <bound> in <cont>`
- array elements: `[<e1>, <e2>, ...]`
- record fields: `{<field1> = <e1>, <field2> = <e2>, ...}`

Note that all those constructs don't necessarily push stuff on the stack or
incur the allocation of a thunk, typically when they're trivial (constants,
variables, etc.). Still, they all formally introduce a new scope.

When entering a scope, the compiler will push the local definitions to the stack
as they come. Those definitions are then cleaned when leaving the scope, which
is either at the end of an expression (a `ret`) or when entering a thunk in head
position, the latter being equivalent to a tail call that can't return (in the
classical assembly sense of returning, not our value-returning `ret`).

When building a thunk, captured variables are copied from the stack to the
closure. If some captured variable come from an even higher scope, it is copied
from the defining scope down to each intermediate scope as a captured variable
as well: for example, in `fun x => let y = 1 in fun _ => let z = y + 1 in z`,
the `x` argument will be copied to both the closure `fun y => let z = y + 1 in
z` and to the closure `y + 1` bound to `z`, to ensure that when we finally get
to build `y + 1`, `x` is still alive.

#### Instruction set

We don't want to fix the instruction set in this proposal, as it will probably
evolve as we implement the bytecode compiler and the virtual machine. The
instruction set should be relatively high-level and standard, handling stack
operations, function application, variable access, jumps and tests, primitive
operation call, closure constructions, thunks update, and primitive operations.

### Intermediate representations

We propose the usage of two intermediate representations.

#### AST

The first one is an AST and would more or less correspond to the current unique
representation, minus runtime-specific constructors. We also intend to make
simplifications by getting rid of runtime efficiency-oriented duplication, such
as `Let`/`LetPattern`, `Record`/`RecRecord`, `Op1/Op2/OpN`, and so on. Records
can also simply be store as a list of bindings, as most of the time we process
them in the first phase but don't really index into them much.

We could have gone closer to the source language, by delaying some desugaring
currently performed in the parser (for example, have a unified representation of
types and terms), which would be better e.g. for LSP or other potential
downstream consumers of the Nickel parser (such as external tooling). Alas, this
wouldn't be adapted to typechecking. Being the right level for typechecking is
our main concern here. As this AST won't be used for evaluation, we can afford
to drop the `Rc` pointers everywhere and switch to arena allocation, using plain
old references: this solves both the size issue (we can use native reference,
which are both pattern-matching friendly and word-sized) and is also
cache-friendly, as the arena usually uses a bump allocator underneath and the
AST is often traversed in the same order than it is allocated.

#### Bytecode

The second representation is a compact and flat (array-like) representation.
This code is composed of instructions, that will be interpreted by the virtual
machine, heap-allocated values and additional metadata (span, etc.). The memory
representation of those components is detailed in later sections.

We will use a simple unsigned integer type to represent code addresses. To avoid
the confusion with native pointers, we will simply call them indices instead of
pointer in the rest of this document.

### Memory representation of values

#### Uniform representation

Here is a list of Nickel values that we need to represent at runtime:

- Null
- Booleans
- Number (54 bytes on 64bits arch)
- String (24 bytes on 64bits arch)
- Sealing Key
- Foreign value (opaque)
- Enum variant
- Record
- Array
- Label
- Custom contract
- Function

The only primitive values that are small enough to be inlined are `null` and
`boolean`. Other values are boxed, represented as a pointer to a block. We can
use the least significant bit of the pointer (pointer tagging) on any
architecture where pointers are at least 2-bytes aligned (every architecture
that matters for Nickel in practice) to inline `null` and booleans.

There is plenty of space remaining for non-pointers (`wordsize-1` bits,
precisely). We can thus inline more things in the future, such as empty records
and arrays to avoid heap allocation and other such special cases, inline small
strings, sealing key as well (we don't expect them to take up more than one
byte), maybe small integers, and so on.

In practice, the pointer is a reference-counted pointer (`Rc<_>` in Rust) that
goes to a block of memory of variable size, with a 1-word header holding the
discriminant (and possibly more information if needed), followed by the span and
then the actual data. In total, accounting for `Rc`, there will be two words
(the weak and the strong counter) before the header, so the global layout of a
block is:

```text
| handled by std::rc::Rc  | actual block            |
-----------------------------------------------------
| weak (1w) | strong (1w) | head (1w) | span | data |
-----------------------------------------------------
```

In a way, ignoring the inlined values, our main representation is one big enum
of variants with exactly one argument that is pointer. In this specific case, we
can use the proposed optimized representation where the tag is stored alongside
the pointee, instead of alongside the pointer as with the standard Rust enum
representation. This saves a lot of space at the cost of making the pointee
dynamically-sized, which is less nice to handle in Rust but isn't a blocker
either.

#### Records and arrays

We leave the precise representation of records and arrays open for now. Their
actual representation doesn't impact the rest of the proposal and can be
optimized at will in the future. The current representation works fine.

#### Closures and thunks

We use a straightforward representation of closures shared by most VMs: a status
flag (see below), a code pointer and an array of captured variables which
corresponds to the free variables of the closure. We don't use Lua's upvalues
for the time being: it's more complex than blindly copying what's needed to the
closure's environment and it's not clear that it's faster in our case (given
that we have 1-words values). Lua-style upvalues might be explored and
benchmarked later.

We don't follow Haskell self-updating model for two reasons.

1. As opposed to Haskell, we have a separate representation for data and code. A
   Haskell value is always an ADT constructor, and the only way to scrutinize it
   is via a (primitive) `case` expression, which lends itself well to data as
   code. The code for a specific constructor `A` called upon `case A(x,y) of
   ...` will load `x` and `y` in the right registers, and jumps to the
   corresponding branch. Nickel has many primitive datatype beyond ADTs and they
   have each several destructors beyond pattern matching, so this approach is
   not really applicable.
2. Haskell compiles to native code, meaning that the closure's entry code can do
   pretty much anything. On the other hand, we are designing a virtual machine
   where operations are limited to the available instruction set, which we want
   to be relatively high-level. The self-updating model requires the closure's
   code to have the required instructions to do, which also needs to be
   interpreted (meaning a larger instruction set and an interpretative
   overhead). We'd rather keep the update process built-in

We thus propose a more traditional cell model with a status flag and either a
code index (unevaluated) or a value pointer (evaluated). Again, we can tag
pointers and encode the whole header on one word - as we control indices and can
make them a few bits smaller than native pointers if required.

The layout would be:

```text
Evaluated
------------------------------------------------
value pointer (ends in 0) | captured variables |
------------------------------------------------

Other status (suspended, locked, blackholed, etc.)
----------------------------------------------------------------------------
Status (n bits <= 3) | code index (wordsize - n bits) | captured variables |
----------------------------------------------------------------------------
```

Functions aren't special; after all, a thunk is just a zero-ary function. They
are represented the same way, but will be compiled with a prelude handling in
particular the argument satisfaction check, required for partial application.

##### Partial application

Partial application requires a bit more machinery. We mostly take inspiration
from the STG machine. When a function is called, it must perform the argument
satisfaction check: verifies that there are enough argument on the stack.

If there are enough arguments, the function is fully applied and we can proceed
with the code.

If there are too few, it's a partial application. We need to build a new closure
object with the available arguments put in the closure's environment, and update
the original closure with an indirection to the new closure. The code of the
updated closure will need to recover the previous arguments from the environment
and do some stack shuffling to get the expected layout for the full application.

For example, imagine that `std.array.map` is strict in its function argument
(it's not in reality, but that's unimportant), then `let f = (+) 2 in [1,2,3] |>
map f |> at 0` will call `(+)` with only one argument. This will fail the
argument satisfaction check, and update `f` with a partial application closure
with `2` in its environment. Then, `at 0` will cause `f 1` to be evaluated,
calling to the closure again. `1` will be on the stack. We need to recover the
stack `START | 1 | 2 |`, which is achieved by just pushing `2` from the
environment to the stack and continuing with the original code for `(+)`.

## Temporary: notes

- 2 ASTs: one similar to the current but arena-allocated, and bytecode. The idea
    is to avoid too many lowering phases, and because we don't really have much
    transformations or optimizations to do on the AST.
- Closure representation: although this looks simplistic, Peyton-Jones argues in
    STG-machine that copying the upvalues into the closure is actually one of
    the most efficient in practice (smarter strategies don't gain much time and
    can cost a lot of memory). So we should probably just do basic closure
    conversion. Not sure the Lua approach is really useful as a first shot,
    because as I understand it, you need to produce the upvalue move instruction
    when a scope ends, and it adds one indirection. Depends on the size of our
    values, I guess - Lua has "large values" thus tries to avoid undue copying.
- Record representation: it's honestly a bit irrelevant, because it can evolve
    independently from the bytecode instructions and other representations.
    Maybe we should use persistent data structure? Still, there might be some
    specific stuff related to the recursive environment and the "lazyness" of
    the recursive environment. Maybe compile that in the same way as OCaml
    mutually recursive closures.
    Should we have, as in JavaScript, a inline representation for small maps?
- Array repr: todo. RPDS?
- Stack elements repr (equality, deep_sequing, argument tracking and other
    specifities of Nickel).
- Number of stacks? Registers? Probably one stack. Should we have a bunch of
    registers as in Lua?
- Instruction set: the Tvix one looks like the more adapted to our case.
- Scope management: stack-allocate let-bindings locally, as in Tvix, but we need
    to clean them. Most probably, a cleaning is mandated when entering a thunk,
    as this is equivalent to a tail call and the environment is discarded.

### Optimizations

- Haskell's update flag: avoid creating a thunk in the first place when in
  normal form or when we can prove that the thunk is only used once
- multi-ary merging (in `a & b & c`, instead of evaluating `r1 <- a & b`, then
    `r2 <- c`, then `r1 & r2`, use some `merge_n` primitive).
