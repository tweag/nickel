# Partial parsing

When providing completions, `nls` would like to parse incomplete nickel expressions. For
example, if the editor contains

```nickel
let x = { foo = 1, bar = 2 } in
let a = x.
```

or

```nickel
let x = { foo = 1 } in
let a = (x & {bar = 2}).
```

and the cursor is at the end, we would like to offer "foo" and "bar" as completions.
Unfortunately the lalrpop parser is not particularly good at recognizing these incomplete
inputs. `nickel pprint-ast` outputs `let x = { foo = 1, } in %<PARSE ERROR>` on the second
example, which is not much use for the completion engine.

## The current state of `nls`

Right now, `nls` provides completion for record fields by running an ad-hoc scan backwards
from the cursor. It looks for `FieldPath`-like tokens, so in the first example it finds "x.".
It then searches for completions based on analysis of the part of the file that parsed
successfully; in the first example, this is `let x = { foo = 1, bar = 2 } in`,
and so `nls` correctly suggests `foo` and `bar` as completions.

Unfortunately, this approach doesn't work in the second example, because
`(x & {bar = 2}).` doesn't look like a `FieldPath`. A similar (but easier to fix) failure
occurs with [paths that have quoted elements](https://github.com/tweag/nickel/issues/1045).

## How to improve?

Here are a few possible approaches to improve the situation:

### Try 1: enrich the lalrpop grammar

We can enlarge the lalrpop grammar so that it can better represent incomplete input.
This would not require modifying the main nickel parser (much like how the repl parser
is a small extension of the main parser). The idea would be to run the main nickel parser
as usual; if a completion is requested inside a parse error, run the incomplete
parser just on that slice of the input. This partial input would not get fed to the typechecker,
but `nls` would inspect it and apply some heuristics to generate completions.

For example, we can add rules like

```
PartialUniTerm =
  | PartialInfixExpr
  | "let" Ident "=" PartialTerm

PartialInfixExpr =
  | PartialAtom
  | InfixExpr "+" PartialInfixExpr

PartialAtom =
  | "(" Term
  | "(" PartialTerm
  | "[" (Term,)*
  | "[" (Term,)* PartialTerm
```

I've made a bit of progress on this, and it seems *possible*, but it will
require duplicating a large part of the original grammar. It's a little
tricky to ensure that the grammar is conflict-free; the key seems to be
ensuring that `PartialFoo` tokens never match a complete `Foo` token.

The other problem with this approach is that it only attempts to handle
incomplete-but-otherwise-valid input. For example, it will fail on

```nickel
let x = { foo = 1, bar = 2 } in
let y
let a = x.
```

Pros:
- I'm not sure at this point. Even getting this to the PoC stage will be more
  work than I had originally anticipated.

Cons:
- Not super robust.
- Lots of duplicated effort.

### Try 2: use a heuristic to search backwards for a valid term

This is a slightly more principled extension to the current approach:
starting from the cursor, search backwards until we find something like
a valid nickel term and then run the real nickel parser on it. This could
lex the input before searching backwards so that it can avoid embedding
a hacky lexer.

Possible stopping heuristics include

- stop when you see an unbalanced "(" or "{";
- stop when you see a "," because in a list or record previous entries
  don't help you suggest completions (well, maybe in a recursive record...);
- stop when you see an "=";
- stop when you see a binary op because the left hand side won't help you complete
  on the right hand side;
- if you see a ")", find the matching "(" and see if the part in between parses; if
  it does, take it all and keep scanning backwards from the "(".

Pros:
- It's a more principled extension of the current approach, so it's probably the least
  work of any of the options here.

Cons:
- There will inevitably be many heuristic choices that affect the reliability of the result.

### Try 3: use treesitter

The treesitter parser does a little better than the lalrpop one. On the second
example input, it parses the second line as containing a pattern (the `y`)
and a "record_operand" (the `(x & {bar = 2}).`). This is probably already
enough information for the completer.

There are two sub-options here: we could use the existing nickel parser as we do today
and then run a tree-sitter parser *in addition* for completion, or we could try to run
only the tree-sitter parser and figure out some way to turn its output into an AST for
the typechecker/linearizer.

Pros:
- It seems fairly principled, assuming treesitter's error recovery is principled.
- The tree-sitter grammar is already there and we presumably already care about maintaining it.

Cons:
- It adds extra dependencies to nls.
- We might have to parse twice.

### Try 4: port treesitter's magic sauce to lalrpop

I don't know what treesitter does to produce those nice error-resilient parses, but
presumably lalrpop could be modified to do something similar. They're both LR parser
generators, after all. Tree-sitter provides some
[references](https://tree-sitter.github.io/tree-sitter/#underlying-research) that I haven't
read yet.

Pros:
- Like the previous option, but there's only one parser.
- It might be fun.

Cons:
- Probably a bunch of work.