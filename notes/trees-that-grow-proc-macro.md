As Nickel grows, it becomes unwieldy to use a single abstract syntax tree (AST) for every stage of the pipeline. Some data are kept out of the AST, even though they would be useful in some places, because they don't make sense and would get in the way at others (e.g. types inferred during typechecking for the LSP, a closure - i.e. a term together with its environment - at runtime). Other data placeholders are in the AST, but aren't actually filled in until later on in the process (e.g. forall's var_kind). This causes `unwrap()`s and `unreachable!()`s and hurts code maintainability.

## Strategies

One strategy to deal with this is to repeat a large AST enum in multiple places with small differences, and convert from one to another as we move through program manipulations. This creates a large maintenance burden, since many changes have to be propagated to all the trees.

Another strategy is outlined by the paper [Trees that Grow](https://www.microsoft.com/en-us/research/uploads/prod/2016/11/trees-that-grow.pdf). This describes an extendable tree structure using type families and pattern synonyms for ergonomics. While this is quite a bit DRYer, there is still a decent amount of boilerplate, and actually using these trees is less intuitive. Especially in Rust, where there are no pattern synonyms (the paper is targeting Haskell).

I (@Radvendii) have explored using proc macros to get the best of both worlds, and remove the boilerplate along the way. There are three ways we could use proc macros, but they share a common syntax.

## Macro Syntax

The naming of things is up in the air, but the general idea is to annotate the tree definition with how fields get modified in the extensions. There are two kinds of extensions, product extensions which add data to an existing term type, and sum extensions which add a new term type. This is the example from the paper, along with their `TC` (typechecking) extension, and `PE` (partial evaluation) extension.

```rust
type Var = String;
enum Typ {
    Int,
    Fun(Box<Typ>, Box<Typ>),
}
enum Val {
    ...
}

#[extendable]
enum Exp {
    Lit(i32),
    Var(Var),
    // type-annotated expression
    Ann(Box<Exp>, Typ),
    // lambda
    Abs(Var, Box<Exp>),
    // function application
    #[prod_extension(TC(Typ))]
    App(Box<Exp, Box<Exp>),
    /// evaluated term
    #[sum_extension(PE)]
    Evl(Val)
}
```

## The Transformations

As mentioned, there are three possibilities for what this could be transformed into. One is just simple trees, the way they would appear if one were hand-writing multiple ASTs:

```rust
enum ExpBase {
    Lit(i32),
    Var(Var),
    Ann(Box<ExpBase>, Typ),
    Abs(Var, Box<ExpBase>),
    App(Box<ExpBase, Box<ExpBase>),
}

enum ExpTC {
    Lit(i32),
    Var(Var),
    Ann(Box<ExpTC>, Typ),
    Abs(Var, Box<ExpTC>),
    App(Typ, Box<ExpTC, Box<ExpTC>),
}

enum ExpPE {
    Lit(i32),
    Var(Var),
    Ann(Box<ExpPE>, Typ),
    Abs(Var, Box<ExpPE>),
    App(Box<ExpPE, Box<ExpPE>),
    Evl(Val),
}
```

This can be used as follows

```rust
fn foo(term: ExpPE) {
    match term {
        ExpPE::Lit(l) => ...,
        ExpPE::Var(v) => ...,
        ExpPE::Ann(e, t) => ...,
        ExpPE::Abs(v, e) => ...,
        ExpPE::App(f, a) => ...,
        ExpPE::Evl(v) => ...,
    }
}
```

But note that if we want to implement similar functions for all of the tree types, we have to implement them for each type separately.

Alternatively, we could generate trees-that-grow-style trees.

```rust
// uninhabited type. cannot be constructed.
pub enum Void {}
trait Extension {
    type Lit;
    type Var;
    type Ann;
    type Abs;
    type App;
    type More;
}
enum Exp<Ext: Extension> {
    Lit(Ext::Lit, i32),
    Var(Ext::Var, Var),
    Ann(Ext::Ann, Box<Exp<Ext>>, Typ),
    Abs(Ext::Abs, Var, Box<Exp<Ext>>),
    App(Ext::App, Box<Exp<Ext>, Box<Exp<Ext>>),
    Ext::More,
}
pub struct Base;
pub struct TC;
pub struct PE;
enum PEMore {
    Evl(Val),
}
impl Extension for Base {
    type Lit = ();
    type Var = ();
    type Ann = ();
    type Abs = ();
    type App = ();
    type More = Void;
}

impl Extension for TC {
    type Lit = ();
    type Var = ();
    type Ann = ();
    type Abs = ();
    type App = Typ;
    type More = Void;
}
impl Extension for PE {
    type Lit = ();
    type Var = ();
    type Ann = ();
    type Abs = ();
    type App = ();
    type More = PEMore;
}
```

Which can be used as follows

```rust

fn foo_tc(term: Exp<TC>) {
    match term {
        Exp::Lit((), l) => ...,
        Exp::Var((), v) => ...,
        Exp::Ann((), e, t) => ...,
        Exp::Abs((), v, e) => ...,
        Exp::App(t, f, a) => ...,
        Exp::Evl(_) => unreachable!(),
    }
}

fn foo_pe(term: Exp<PE>) {
    match term {
        Exp::Lit((), l) => ...,
        Exp::Var((), v) => ...,
        Exp::Ann((), e, t) => ...,
        Exp::Abs((), v, e) => ...,
        Exp::App((), f, a) => ...,
        Exp::Evl(PEMore::Evl(v)) => ...,
    }
}

impl<Ext> Display for Exp<Ext> where
Ext: Extension + Display {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      todo!()
    }
}

impl Display for PE {
    ...
}
impl Display for TC {
    ...
}
```

Note that we can use the generic `Exp::` namespace, and we can implement traits generically over the extension, but that matching on and constructing terms is cumbersome, and requires extra parameters.

A third option, which combines some of the pros and cons of each, is to generate both, as well as `From` implementations in both directions. This allows us to work most of the time with simple trees that are easier to understand, while converting to extensions when we need to use generic functions. There are a number of ways we could structure this, but I've decided to put the definitions behind a `mod`, so we can use generic names like `Extension` without fear of namespace conflicts.


```rust
pub mod exp {
    use super::*;
    pub enum Base {
        Lit(i32),
        Var(Var),
        Ann(Box<Base>, Typ),
        Abs(Var, Box<Base>),
        App(Box<Base>, Box<Base>),
    }
    pub enum TC {
        Lit(i32),
        Var(Var),
        Ann(Box<TC>, Typ),
        Abs(Typ, Var, Box<TC>),
        App(Box<TC>, Box<TC>),
    }
    pub enum PE {
        Lit(i32),
        Var(Var),
        Ann(Box<PE>, Typ),
        Abs(Var, Box<PE>),
        App(Box<PE>, Box<PE>),
        Evl(Val),
    }
    pub trait Extension {
        type Lit;
        type Var;
        type Ann;
        type Abs;
        type App;
        type More;
    }
    pub enum ThatGrows<Ext: Extension> {
        Lit(Ext::Lit, i32),
        Var(Ext::Var, Var),
        Ann(Ext::Ann, Box<ThatGrows<Ext>>, Typ),
        Abs(Ext::Abs, Var, Box<ThatGrows<Ext>>),
        App(Ext::App, Box<ThatGrows<Ext>>, Box<ThatGrows<Ext>>),
        More(Ext::More),
    }
    pub mod ext {
        use super::{super::*, *};
        pub enum Void {}
        pub struct Base;
        impl Extension for Base {
            type More = Void;
            type Lit = ();
            type Var = ();
            type Ann = ();
            type Abs = ();
            type App = ();
        }
        pub struct PE;
        pub enum PEMore {
            Evl(Val),
        }
        impl Extension for PE {
            type Lit = ();
            type Var = ();
            type Ann = ();
            type Abs = ();
            type App = ();
            type More = PEMore;
        }
        pub struct TC;
        impl Extension for TC {
            type Lit = ();
            type Var = ();
            type Ann = ();
            type Abs = Typ;
            type App = ();
            type More = Void;
        }
    }
    impl From<Base> for ThatGrows<ext::Base> {
        fn from(value: Base) -> Self {
            match value {
                Base::Lit(a_0) => ThatGrows::Lit((), a_0),
                Base::Var(a_0) => ThatGrows::Var((), a_0),
                Base::Ann(a_0, a_1) => ThatGrows::Ann((), Box::new((*a_0).into()), a_1),
                Base::Abs(a_0, a_1) => ThatGrows::Abs((), a_0, Box::new((*a_1).into())),
                Base::App(a_0, a_1) => {
                    ThatGrows::App((), Box::new((*a_0).into()), Box::new((*a_1).into()))
                }
            }
        }
    }
    impl From<PE> for ThatGrows<ext::PE> {
        fn from(value: PE) -> Self {
            match value {
                PE::Lit(a_0) => ThatGrows::Lit((), a_0),
                PE::Var(a_0) => ThatGrows::Var((), a_0),
                PE::Ann(a_0, a_1) => ThatGrows::Ann((), Box::new((*a_0).into()), a_1),
                PE::Abs(a_0, a_1) => ThatGrows::Abs((), a_0, Box::new((*a_1).into())),
                PE::App(a_0, a_1) => {
                    ThatGrows::App((), Box::new((*a_0).into()), Box::new((*a_1).into()))
                }
                PE::Evl(a_0) => ThatGrows::More(ext::PEMore::Evl(a_0)),
            }
        }
    }
    impl From<TC> for ThatGrows<ext::TC> {
        fn from(value: TC) -> Self {
            match value {
                TC::Lit(a_0) => ThatGrows::Lit((), a_0),
                TC::Var(a_0) => ThatGrows::Var((), a_0),
                TC::Ann(a_0, a_1) => ThatGrows::Ann((), Box::new((*a_0).into()), a_1),
                TC::Abs(a_0, a_1, a_2) => ThatGrows::Abs(a_0, a_1, Box::new((*a_2).into())),
                TC::App(a_0, a_1) => {
                    ThatGrows::App((), Box::new((*a_0).into()), Box::new((*a_1).into()))
                }
            }
        }
    }
    impl From<ThatGrows<ext::Base>> for Base {
        fn from(value: ThatGrows<ext::Base>) -> Self {
            match value {
                ThatGrows::Lit((), a_0) => Base::Lit(a_0),
                ThatGrows::Var((), a_0) => Base::Var(a_0),
                ThatGrows::Ann((), a_0, a_1) => Base::Ann(Box::new((*a_0).into()), a_1),
                ThatGrows::Abs((), a_0, a_1) => Base::Abs(a_0, Box::new((*a_1).into())),
                ThatGrows::App((), a_0, a_1) => {
                    Base::App(Box::new((*a_0).into()), Box::new((*a_1).into()))
                }
                ThatGrows::More(_) => unreachable!(),
            }
        }
    }
    impl From<ThatGrows<ext::PE>> for PE {
        fn from(value: ThatGrows<ext::PE>) -> Self {
            match value {
                ThatGrows::Lit((), a_0) => PE::Lit(a_0),
                ThatGrows::Var((), a_0) => PE::Var(a_0),
                ThatGrows::Ann((), a_0, a_1) => PE::Ann(Box::new((*a_0).into()), a_1),
                ThatGrows::Abs((), a_0, a_1) => PE::Abs(a_0, Box::new((*a_1).into())),
                ThatGrows::App((), a_0, a_1) => {
                    PE::App(Box::new((*a_0).into()), Box::new((*a_1).into()))
                }
                ThatGrows::More(ext::PEMore::Evl(a_0)) => PE::Evl(a_0),
            }
        }
    }
    impl From<ThatGrows<ext::TC>> for TC {
        fn from(value: ThatGrows<ext::TC>) -> Self {
            match value {
                ThatGrows::Lit((), a_0) => TC::Lit(a_0),
                ThatGrows::Var((), a_0) => TC::Var(a_0),
                ThatGrows::Ann((), a_0, a_1) => TC::Ann(Box::new((*a_0).into()), a_1),
                ThatGrows::Abs(a_0, a_1, a_2) => TC::Abs(a_0, a_1, Box::new((*a_2).into())),
                ThatGrows::App((), a_0, a_1) => {
                    TC::App(Box::new((*a_0).into()), Box::new((*a_1).into()))
                }
                ThatGrows::More(_) => unreachable!(),
            }
        }
    }
}
```

This can be used in either of the ways above, and conversion between the forms is as easy as `.into()`. There is some concern about the overhead of allocating an entirely new tree. It would have to be seen how much this is done in practice, since it would only be for functions generic over trees.

@jneem had some thoughts about avoiding that conversion cost by storing references and only converting one level of the tree at a time.

```rust
enum ThatGrows<'a, Ext: Extension, T> {
    Lit(&'a Ext::Lit, &'a i32),
    Var(&'a Ext::Var, &'a Var),
    // If there's recursion, we just take a shallow reference, not a deep copy.
    Ann(&'a Ext::Ann, &'a T, &'a Typ),
    Abs(&'a Ext::Abs, Var, &'a T),
    App(&'a Ext::App, &'a T, &'a T),
    More(&'a Ext::More),
}

impl<'a> From<&'a Base> for ThatGrows<'a, ext::Base, Base> { ... }
impl<'a> From<&'a PE> for ThatGrows<'a, ext::PE, PE> { ... }
impl<'a> From<&'a TC> for ThatGrows<'a, ext::TC, TC> { ... }
```

Then generic functions would be implemented like this

```rust
impl<'a, Ext, T> Display for ThatGrows<'a, Ext, T> where
Ext: Extension + Display,
Exp<'a, Ext, T>: From<T>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // every time we recurse, we call .into()
        todo!()
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
