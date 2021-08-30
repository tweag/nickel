{
  nums = {
    Int
    | doc m#"
      Contract to enforce a number is an integer.

      For example:
      ```nickel
        (1.5 | Int) =>
          error
        (42 | Int) =>
          42
      ```
      "#m
    = fun label value =>
      if %isNum% value then
        if value % 1 == 0 then
          value
        else
          %blame% (%tag% "not an integer" label)
      else
        %blame% (%tag% "not a number" label),

    Nat
    | doc m#"
      Contract to enforce a number is a natural number (including 0).

      For example:
      ```nickel
        (42 | Nat) =>
          42
        (0 | Nat) =>
          0
        (-4 | Nat) =>
          error
      ```
      "#m
    = fun label value =>
      if %isNum% value then
        if value % 1 == 0 && value >= 0 then
          value
        else
          %blame% (%tag% "not a natural" label)
      else
        %blame% (%tag% "not a number" label),

    PosNat
    | doc m#"
      Contract to enforce a number is a positive natural number.

      For example:
      ```nickel
        (42 | PosNat) =>
          42
        (0 | PosNat) =>
          error
        (-4 | PosNat) =>
          error
      ```
      "#m
    = fun label value =>
      if %isNum% value then
        if value % 1 == 0 && value > 0 then
          value
        else
          %blame% (%tag% "not positive integer" label)
      else
        %blame% (%tag% "not a number" label),

    NonZero
    | doc m#"
      Contract to enforce a number is anything but zero.

      For example:
      ```nickel
        (1 | NonZero) =>
          1
        (0.0 | NonZero) =>
          error
      ```
      "#m
    = fun label value =>
      if %isNum% value then
        if value != 0 then
          value
        else
          %blame% (%tag% "non-zero" label)
      else
        %blame% (%tag% "not a number" label),

    isInt : Num -> Bool
    | doc m#"
      Checks if the given number is an integer.

      For example:
      ```nickel
        isInt 42 =>
          true
        isInt 1.5 =>
          false
      ```
      "#m
    = fun x => %isNum% x && (x % 1 == 0),

    min : Num -> Num -> Num
    | doc m#"
      Results in the lowest of the given two numbers.

      For example:
      ```nickel
        min (-1337) 42 =>
          -1337
      ```
      "#m
    = fun x y => if x <= y then x else y,

    max : Num -> Num -> Num
    | doc m#"
      Results in the highest of the given two numbers.

      For example:
      ```nickel
        max (-1337) 42 =>
          42
      ```
      "#m
    = fun x y => if x >= y then x else y,

    floor : Num -> Num
    | doc m#"
      Rounds the number down to the next integer.

      For example:
      ```nickel
        floor 42.5 =>
          42
      ```
      "#m
    = fun x =>
      if x >= 0
      then x - (x % 1)
      else x - 1 - (x % 1),

    abs : Num -> Num
    | doc m#"
      Results in the absolute value of the given number.

      For example:
      ```nickel
        abs (-5) =>
          5
        abs 42 =>
          42
      ```
      "#m
    = fun x => if x < 0 then -x else x,

    fract : Num -> Num
    | doc m#"
      Results in the fractional part of the given number.

      For example:
      ```nickel
        fract 13.37 =>
          0.37
        fract 42 =>
          0
      ```
      "#m
    = fun x => x % 1,

    trunc : Num -> Num
    | doc m#"
      Truncates the given number.

      For example:
      ```nickel
        trunc (-13.37) =>
          -13
        trunc 42.5 =>
          42
      ```
      "#m
    = fun x => x - (x % 1),

    pow : Num -> Num -> Num
    | doc m#"
      `pow x y` result in `x` to the power of `y`.

      For example:
      ```nickel
        pow 2 8 =>
          256
      ```
      "#m
    = fun x n => %pow% x n,
  }
}
