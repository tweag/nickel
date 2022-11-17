# Revertible thunks: but why?

```nickel
let x = {
  foo = 1,
  bar = foo + 1,
} in
x.bar + (x & {foo | force = 2}).bar # 5
```

## Using normal thunks

If we just use "normal" thunks, but "tie the knot" (building the recursive
environment and put other fields in scope).

```nickel
let x = {
  foo = 1,
  bar = foo + 1,
} in
(x & {foo | force = 2}).bar # That would work: 3
```

```nickel
let x = {
  foo = 1,
  bar = foo + 1,
} in
x.bar + (x & {foo | force = 2}).bar # That wouldn't: 4
```

## Using functions

For each record operation, like field access, we applyu the function again:

```nickel
x_encoded = fun self => {
  foo = 1,
  bar = self.foo + 1,
}

# record application
(x.field ~ let rec x = x_encoded x in x.field)
```

**Remark**: in a function representation for recursive records, `self` can only
be changed upon merging.



## Objects digression

```java
public class X {
  @override
  public int foo() {
    return 1;
  }

  public int bar() {
    return this.foo() + 1
  }
}

public class XExtended extends X {
  public int foo() {
    return 2;
  }
}
```

(Closures are poor man's objects are recursive records...)


```nix
let x_encoded = self: {
    foo = 1;
    bar = self.foo + 1;
}; in
let rec x = x_encoded x; in
let rec x_extended = (x_encoded (x // {foo = 2;})) // {foo = 2;}; in
x.bar + x_extended.foo
```

```nickel
let x_encoded = {
  foo = 1,
  bar = foo + 1,
}
```


