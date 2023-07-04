As Nickel grows, it becomes unwieldy to use a single abstract syntax tree (AST) for every stage of the pipeline. Some data are kept out of the AST, even though they would be useful in some places, because they don't make sense and would get in the way at others (e.g. types inferred during typechecking for the LSP, a closure - i.e. a term together with its environment - at runtime). Other data placeholder are in the AST, but aren't actually filled in until later on in the process (e.g. forall's var_kind). This causes `unwrap()`s and `unreachable!()`s and hurts code maintainability.

## Strategies

One strategy to deal with this is to repeat a large AST enum in multiple places with small differences, and convert from one to another as we move through program manipulations. This creates a large maintenance burden, since many changes have to be propagated to all the trees.

Another strategy is outlined by the paper [Trees that Grow](https://www.microsoft.com/en-us/research/uploads/prod/2016/11/trees-that-grow.pdf). This describes an extendable tree structure using type families and pattern synonyms for ergonomics. While this is quite a bit DRYer, there is still a decent amount of boilerplate, and actually using these trees is less intuitive. Especially in Rust, where there are no pattern synonyms (the paper is targeting Haskell).

I (@Radvendii) have explored using proc macros to get the best of both worlds, and remove the boilerplate along the way. There are three ways we could use proc macros, but they share a common syntax.

## Macro Syntax

The naming of things is up in the air, but the general idea is to annotate the tree definition with how fields get modified in the extensions. There are two kinds of extensions, product extensions which add data to an existing term type, and sum extensions which add a new term type.

```rust
#[extendable]
enum Term {
    NoChange(),
    #[prod_extension(Foo(u8))]
    Extend(i32),
    #[sum_extension(Foo)]
    Add(Blah),
}
```

## The Transformations

As mentioned, there are three possibilities for what this could be transformed into. One is just simple trees, the way they would appear if one were hand-writing multiple ASTs:

```rust
enum TermBase {
  NoChange(),
  Extend(i32),
}

enum TermFoo {
  NoChange(),
  Extend(u8, i32),
  Add(i32)
}
```

Alternatively, we could generate trees-that-grow-style trees.

```rust
pub enum Void {}
trait Extension {
    type NoChange;
    type Extend;
    type More;
}
enum Term<Ext: Extension> {
    NoChange(Ext::NoChange),
    Extend(Ext::Extend, i32),
    More(Ext::More),
}
pub struct Foo;
enum FooMore {
    Add(Blah),
}
impl Extension for Foo {
    type Extend = HashSet<Blah>;
    type More = FooMore;
    type NoChange = ();
}
struct Base;
impl Extension for Base {
    type More = Void;
    type NoChange = ();
    type Extend = ();
}
```

A third option, which combines some of the pros and cons of each, is to generate both, as well as `From` implementations in both directions. This allows us to work most of the time with simple trees that are easier to understand, while converting to extensions when we need to use generic functions. There are a number of ways we could structure this, but I've decided to put the definitions behind a `mod`, so we can use generic names like `Extension` without fear of namespace conflicts.


```rust
pub mod term {
    use super::*;
    pub enum Base {
        NoChange(),
        Extend(i32),
    }
    pub enum Foo {
        Extend(HashSet<Blah>, i32),
        Add(Blah),
        NoChange(),
    }
    pub trait Extension {
        type NoChange;
        type Extend;
        type More;
    }
    pub enum ThatGrows<Ext: Extension> {
        NoChange(Ext::NoChange),
        Extend(Ext::Extend, i32),
        More(Ext::More),
    }
    pub mod ext {
        use super::{super::*, *};
        pub enum Void {}
        pub struct Foo;
        pub enum FooMore {
            Add(Blah),
        }
        impl Extension for Foo {
            type NoChange = ();
            type Extend = HashSet<Blah>;
            type More = FooMore;
        }
        pub struct Base;
        impl Extension for Base {
            type NoChange = ();
            type Extend = ();
            type More = Void;
        }
    }
    impl From<Foo> for ThatGrows<ext::Foo> {
        fn from(x: Foo) -> Self {
            match x {
                Foo::NoChange() => ThatGrows::NoChange(()),
                Foo::Extend(a_0, a_1) => ThatGrows::Extend(a_0, a_1),
                Foo::Add(a_0) => ThatGrows::More(ext::FooMore::Add(a_0)),
            }
        }
    }
    impl From<Base> for ThatGrows<ext::Base> {
        fn from(x: Base) -> Self {
            match x {
                Base::NoChange() => ThatGrows::NoChange(()),
                Base::Extend(a_0) => ThatGrows::Extend((), a_0),
            }
        }
    }
    impl From<ThatGrows<ext::Foo>> for Foo {
        fn from(x: ThatGrows<ext::Foo>) -> Self {
            ThatGrows::NoChange(()) => Foo::NoChange(),
            ThatGrows::Extend(a_0, a_1) => Foo::Extend(a_0, a_1),
            ThatGrows::More(ext::FooMore::Add(a_0)) => Foo::Add(a_0),
        }
    }
    impl From<ThatGrows<ext::Base>> for Base {
        fn from(x: ThatGrows<ext::Base>) -> Self {
            ThatGrows::NoChange(()) => Base::NoChange(),
            ThatGrows::Extend((), a_1) => Base::Extend(a_1),
            ThatGrows::More(_) => unreachable!(),
        }
    }
}
```

## Comparison

| AST Strategy → <br> Pros/Cons ↴               | Single AST | Multiple ASTs | Trees that Grow | Proc Macro (Simple) | Proc Macro (Just TtG) | Proc Macro (Simple + TtG) |
|---------------------------------              |------------|---------------|-----------------|---------------------|-----------------------|---------------------------|
| accurately represents structure at each stage | No         | Yes           | Yes             | Yes                 | Yes                   | Yes                       |
| DRY                                           | Yes        | No            | Boilerplate     | Yes                 | Yes                   | Yes                       |
| Non-centralized AST variant definitions       | N/A        | Yes           | Yes             | No                  | No                    | No                        | 
| Generic functions over tree types             | Yes        | No            | Yes             | No                  | Yes                   | Yes                       |
| Clean `match`ing                              | No         | Yes           | No              | Yes                 | No                    | Yes                       |
| No confusing type family nonsense             | Yes        | Yes           | No              | Yes                 | No                    | Most of the time          |
| No proc macro nonsense                        | Yes        | Yes           | Yes             | No                  | No                    | No                        |

## Prior art

- Trees that grow: https://www.microsoft.com/en-us/research/uploads/prod/2016/11/trees-that-grow.pdf
- Someone else translated it to rust (without macros, or simple enums): https://github.com/guygastineau/rust-trees-that-grow/tree/main
