# Typing in Nickel

## Preamble

Static typing or dynamic typing? This is the eternal debate of languages
aficionados. While the idea that static typing pretty much a necessity to
maintain mental sanity while working on large codebases in a general purpose
programming language goes a long way, the situation for an interpreted
configuration language, which shares some commonalities with scripting, appears
less clear-cut.

Nevertheless, whoever has ever faced puzzling dynamic type errors (as in
[Nix]()) with messages far removed from both the actual issue and offending code
may have felt the need for some form of typing, even in the most dynamic
language. This is especially true when working with higher-order functions, that
tends to delay type errors by passing values with the wrong shape around until
it breaks in a totally different location.

On the other hand, for pure configuration code, static typing is less useful.
First, a configuration is a terminating program run once on fixed inputs: here,
basic type errors will show up at evaluation anyway. What's more, Nickel has a
powerful validation system, call contracts, that can already do more in an
easier way.

For those reasons, Nickel takes an hybrid approach to typing: *gradual typing*.

## Untyped by default

By default, Nickel code is run in *untyped* mode (understand dynamically typed).

Ex:

While this is fine for configuration code, in particular when checked by a
contract, it doesn't work well once we are using functions.

Ex: flatten

This example shows how dynamic typing delays type error, making them harder to
fix. Here, `flatten` is totally fine, but the error points at its
implementation. The problem is actually that the caller provided an argument of
the wrong type: the last element should be a list, but it's a number. Alas, the
user (who is most probably not even the author of `flatten`) would have a hard
time reaching this conclusion from the error in a real situation.

## Typed blocks

This is precisely the poster card use case for static typing. The typechecker
catch the error earlier, seeing that the type expected by `flatten` and the one
of the argument don't match.

To use static typing in Nickel, we use `:` to introduce a *type annotation*.
This annotation switches the typechecker on for the annotated expression: a
variable definition, a record field or any expression using inline annotations.

Examples:

Let us try on the flatten example. We want the call to be inside the typechecked
block. The easiest way is to add an annotation at the top-level:

Example

That's already much better! The error now points at the call site. What's more,
we just had to give the top-level annotation. Nickel uses a form of
unification-based type inference, which makes it able to infer the type of e.g.
`flatten` without help. Although there's a twist about so called polymorphism
(aka generic functions), but we'll come back to this later.

For now, let us have a quick tour of the type system.

### Types

The basic types are:

- `Dyn`: the dynamic type. This is the type given to any expression that is not
  in a typed block. A value of type `Dyn` can be pretty much anything.
- `Num`: the only number type. Currently implemented as 64bits floats
- `Str`: strings
- `Bool`: booleans, `true` or `false`
<!-- - `Lbl`: a contract label. You usually don't need to use it or worry about it, -->
<!--     it is more of an internal thing.  -->

You can use the following type constructors:

- **List**: `List T`. A list of elements of type `T`. When no `T` is specified, `List`
    alone is an alias for `List Dyn`.
    Ex: `let x : List (List Num) = [[1,2], [3,4]] in lists.flatten x`
- **Record**: `{field1: T1, .., fieldn: Tn}`. A record whose fields
    (known statically) are `field1`, .., `fieldn` respectively of type `T1`, ..,
    `Tn`.
- **Dynamic record**: `{_: T}`. A record whose fields
    (unknown statically) are all of type `T` but can have arbitrary names.
    Typically used to model dictionaries. 
    Ex: `let occurences : {_: Num} = {a = 1, b = 3, c = 0} in records.map (fun
    char count => count+1)`
- **Enum**: `<tag1, .., tagn>`: an enumeration comprised of alternatives `tag1`, ..,
    `tagn`. An enumeration value is prefixed with a backtick and serialized as a
    string. It is useful to encode finite alternatives. The advantage over
    strings is that the typechecker handles them more finely: it is able to
    detect incomplete matches for example.
    Ex: 
    ```
    let protocol : <http, ftp, sftp> = `http in
    switch {
      http -> 1,
      ftp -> 2,
      sftp -> 3
    } protocol : Num
    ```
- **Arrow (function)**: `S -> T`. A function taking arguments of type `S` and returning a value of
    type `T`. For multi-parameters functions, just iterate the arrow constructor.
    Ex: 
    ```
    {
      incr : Num -> Num = fun x => x + 1,
      mkPath : Str -> Str -> Str -> Str = fun basepath filename ext =>
        "#{basepath}/#{filename}.#{ext}",
    }
    ```

#### Polymorphism


