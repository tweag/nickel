# Type vs contracts: when to?

You are writing Nickel code and wonder how it should be annotated. Leave it
without annotation ? Add type annotations? Use contracts? Here is a quick guide
when you don't know what to do!

If your expression is:

## A function

Most of the time, you should use **type annotations** for functions. Typing is
most often more adapted than contracts for functions. You can exceptionally
use contracts when types are not expressive enough to encode the property you
want (as in `#ValidUrl -> #Port -> #ValidUrl`) or if the type system is not
powerful enough to see that the code is fine.

Type annotations do have a runtime cost. If you hit contracts-related
performances issues, you can always disable some contract checks using specific
flags. With annotations, code is still typechecked, and you can still turn
contracts checking back on for debugging mean when a problem arises. Without
annotations, you're out of luck.

What to do depends on the context:

- *Anonymous function: nothing*. Short functions outsife of a let-binding can
    usually live without annotation. Inside a typed block, they will be
    typechecked anyway. Outside, they won't be reused elsewhere, and are
    generally small functions passed as arguments to others typed higher-order
    functions, that will apply a guarding contract.

    Example: `lists.map (fun x => x + 1) [1,2,3]`
- *Let-bound function outside of typed block: use a type annotation.* Even if
    local to a file, if your function is bound to a variable, it means it can 
    be potentially used in several places.

    Example: `let appendTm: Str -> Str = fun s => s ++ "(TM)" in ...`
- *Let-bound function inside a typed block: nothing or type annotation*. Inside a
    typed block, types are inferred, so it is OK for simple functions to not be
    annotated. However, you are required to annotate it if it is polymorphic,
    because the typechecker won't infer polymorphic types for you. When the
    function type is non trivial, it can also be better to write an annotation
    for the sake of clarity.

    Example:
    ```nickel
    let foo : Num =
      let addTwo = fun x => x + 2 in
      addTwo 4 
    ```

## Library (record of functions)

You should use **type annoations** for records of functions. Currently Nickel
doesn't have a specific notion of a library or a module. You can just put
functions inside a record. Given the function case, you should also use a type
annotation on your record to make the type of the functions accessible to the
outside. Otherwise, the record is typed as `Dyn` and will shadow the type
information, making your library basically unusable inside typed code.

### Example

DON'T
```
{
  foo :  Num -> Num = fun x => x + 1,
  bar : Num -> Num = foo,
}
```

BUT DO
```
{
  foo = fun x => x + 1,
  bar = foo,
} : {
  foo : Num -> Num,
  bar : Num -> Num,
}
```

Alternatively, you can repeat your types both at the function level
and at the record level. It makes code more navigable and `query`-friendly, but at the expense of
repetition and duplicated contract checks. It is also currently required for
polymorphic functions because of [the following bug](). A better solution will probably be
implemented in the future: type holes.

(NOT YET POSSIBLE)
```
{
  foo : Num -> Num = fun x => x + 1,
  bar : Num -> Num = foo,
} : _
```

## Data (record, list)

Conversely, for data inside configuration code, you should use **contracts**.
Types are not adding much over contracts for configuration data, while contracts
are more flexible and expressive.

Example:
```nickel
let Schema = {
  name | Str
       | doc "Name of the package",
  version | #PkgVersion
          | doc "The semantic version of the package",
          | default = "1.0.0",
  build | List #BuildSteps
        | doc "The steps to perform in order to build the package",
        | default = [],
} in
{
  name = "hello",
  build = [
    command "gcc hello.c -o hello",
    command "mv hello $out"
  ]
} | #Schema
```

## Computation (compound expressions)

Some expressions are neither immediate data nor functions. Take for example the
application of a function such as `lists.map (fun s => "http://#{s}/index")
servers`. Usually, you should do **nothing**.

- *Inside configuration: nothing*. The function or operator you are using should
  be typed, and thus protected by a contract. The final value should also be
  protected by a contract, as per the advice on configuration code. Thus, for a
  simple computation like the example above, it is not necessary to add an
  annotation.
- *Inside typed block: nothing*. Inside a typed block, the application will be
  inferred and typechecked, so you shouldn't have to add anything.

## Debugging

At last, both type annotations and contracts come in handy for debugging.  In
this case, you don't have to follow the previous advices, and you can drop
random annotations or contract applications pretty much everywhere you see fit
to help better locate when something is going wrong.
