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
  alleviate the pressure on the allocator. Of course, there's some trade-off
  here, as reducing the size is often done by adding indirections, but this also
  has a cost at runtime

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
run-time values is already departing from the pure tree-walking interpreter. I
don't think we need to go as far as a full-fledged low-level bytecode compiler
and interpreter, but rather to find a trade-off in-between, where compilation is
a relatively simple and cheap program transformation while still giving some
room for a more optimized runtime representation and a faster evaluation scheme.

## VM examples

This section gathers examples of the virtual machine of real world languages as
a source of inspiration and comparison. Those examples have been selected to
showcase a variety of designs and constraints, from either strict or non-strict
functional languages, statically typed or dynamically typed (thus with similar
challenges regarding the representation of records).

We'll look more specifically into the following aspects:

- the memory representation of values
- the general architecture of the virtual machine (stack-based, register-based,
    representation of the environment, etc.)
- high-level (more instructions, more complex vm, simpler compilation scheme) vs
    low-level

### Lua

Lua is an efficient scripting language. The design of the virtual machine is
intended to provide a lightweight and portable VM where code is both simple and
fast to compile (one-pass) and to run. Indeed Lua is often embedded in other
software projects (games, industrial systems, etc.).

The Lua virtual machine has some optimizations that are quite Lua-specific (Lua
represents everything as dictionaries, including traditional contiguous arrays -
they must optimize such encoding to get good performance), but it is nonetheless
an interesting inspiration as a simple runtime of a dynamically typed language
where dictionaries are ubiquitous (kinda like in Nickel).

#### Memory representation

Lua has 8 primitive types: `nil`, `boolean`, `number`, `string`, `function`,
`table`, `userdata` and `thread`. Values are represented as a tagged union.
`nil`, `boolean` and `number` are represented inline (unboxed). `string`,
`function`, `table` and `userdata` are represented as pointers to the actual
data.

This representation takes between 3 and 4 words per value (depending on the
architecture), which isn't small by VM standards. Smalltalk, for example, uses
spare bits in each pointer to reduce the number of words used, but this isn't
portable or implementable in ANSI C.

Because tables are used to represent arrays, Lua 5 uses a hybrid representation
with an array part and a hash part.

Lua also has a peculiar representation of closures. While compilation of
closures to native code have a vast body of literature and experience in
functional languages, most analyses involved are deemed to expensive or too
complex for a simple one-pass compiler. Instead, in Lua, outer variables are
accessed indirectly through a slot called _upvalue_, which originally points to
the stack of the enclosing function owning the referenced value. When the stack
frame is freed, the value is moved to the slot so that it can outlive the
enclosing function call. Thanks to the additional indirection, this move is
invisible to the closures themselves. This is combined with flat closures
(access to more than one outer level causes the corresponding variable to be
pulled in the enclosing function), and a list of open upvalues to ensure that
there's only on such slot pointing to one given value at all time, even if this
slot is shared by many closures.

#### Virtual machine

The Lua virtual machine is notably register-based since Lua 5.0 (note that
there's still a stack for activation records) while most bytecode VMs out there
are purely stack-based. Registers save many `push` and `pop` operations, which
both avoids a non-trivial amount of copying (especially with the tagged union
representation) and reduces the number of instructions (albeit the instructions
are larger, because they often need additional operands to indicate which
registers they operate on). Registers also reduce the number of instructions
needed: the Lua VM only has 35. The Lua VM has 256 registers represented as an
array at runtime guaranteeing fast access.

#### References

- [The Implementation of LUA 5](https://www.lua.org/doc/jucs05.pdf)

### OCaml

#### Memory representation

OCaml uses a uniform memory representation where any value is represented as a
single machine word. Unboxed values (integers, floats, etc.) are distinguished
from pointers by ther least significant bit (and are thus encoded on `n-1` bits
compared to their, say, C equivalent). This is needed for garbage collection
only.

Boxed values are represented as a pointer to a block, which is a contiguous area
of the memory with a one-word header followed by some arbitrary content, whose
shape depends on the type of the value.

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

### Haskell

Despite not being advertised, Haskell has an interpreter as well, which is used
mostly for the GHCi REPL. This section describes what we know of the actual the
bytecode interpreter, but also incoporates some aspect of the original STG
machine which is the basis of the low-level operational semantics of Haskell.

### Memory representation

Haskell, like Nickel, needs to represent thunks - unevaluated expressions -
which can capture variables from the environment. In some sense, everything is
potentially a closure.

#### References

- [The STG machine](https://dl.acm.org/doi/pdf/10.1145/99370.99385)
- [Bytecode instruction datatype definition in GHC](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/ByteCode/Instr.hs#L60)

### JavaScript (V8)

JavaScript doesn't have one official virtual machine, but rather many different
performance-oriented implementations. We'll focus on the V8 engine, which is one
of the fastest ubiquitous and most used JavaScript engine. V8 is a really
complex JIT compiler, which doesn't really fit the model of a basic bytecode
interpreter. Still, V8 features the Ignition bytecode interpreter as part of its
pipeline and implements noteworthy memory representation optimizations.

#### Memory representation

V8 uses a technique called _fast properties_ to represent objects. One
ingredient is in not very different from the technique used in Lua: have an
array part for contiguous properties and a dictionary part for the rest. We
won't expand on the handling of the array part, which is currently useless for
Nickel, having a first-class arrays.

The dictionary part isn't necessarily represented as a proper hashtable, because
this doesn't play well with inline caching, so V8 tries to avoid them. By
default, initial properties are inlined as an array in the object's data, and a
separate metadata structure - the `HiddenClass` - is used to map names to
offsets (it seems it's just a dictionary with extra steps, but for some reason
this work for inline caching?). Accessing inline properties is the fastest, but
adding and removing them requires to modify both the object and the
`HiddenClass`. At some point it can become too costly to keep doing that, so V8
will switch to a standard self-contained key-value map at some point (_slow
properties_).

#### Virtual machine

V8 is designed to be an optimizing JIT native code compiler. Ignition adds a new
intermediate representation, higher-level than native code but lower-level than
the AST, that serves both as a compact runtime representation that is cheap to
compile to. The bytecode is also designed to be easily specialized to native
code when needed. Finally, the bytecode serves as a single exchange format the
different optimizing compilers.

Ignition's bytecode has been optimized for size more than for speed, the main
motivation being to reduce the memory footprint of the V8 engine. Ignition is
register-based, with an unbounded number of local registers (or so it seems,
which are stored on the local stack of the function - so register are more like
stack slots?). The Ignition interpreter is more like a very lazy compiler:
interpretation is done by generating macro-assembly (a kind of portable assembly
used by the Turbofan optimizing compiler) that is then transpiled by Turbofan to
machine code, for every primitive instruction, on the fly.

#### References

- [V8 documentation: fast properties](https://v8.dev/blog/ignition-interpreter)
- [V8 documentation: Ignition interpreter](https://v8.dev/blog/ignition-interpreter)
- [V8 implementation: list of instructions](https://github.com/v8/v8/blob/master/src/interpreter/bytecodes.h)

### Clean

## Proposal
