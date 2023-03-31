{
  number = {
    Integer
      | doc m%"
        Enforces that a number is an integer.

        # Examples

        ```nickel
        (1.5 | Integer) =>
          error
        (42 | Integer) =>
          42
        ```
      "%
      = fun label value =>
        if %typeof% value == `Number then
          if value % 1 == 0 then
            value
          else
            %blame% (%label_with_message% "not an integer" label)
        else
          %blame% (%label_with_message% "not a number" label),

    Nat
      | doc m%"
        Enforces that a number is a natural number (including 0).

        # Examples

        ```nickel
        (42 | Nat) =>
          42
        (0 | Nat) =>
          0
        (-4 | Nat) =>
          error
        ```
      "%
      = fun label value =>
        if %typeof% value == `Number then
          if value % 1 == 0 && value >= 0 then
            value
          else
            %blame% (%label_with_message% "not a natural" label)
        else
          %blame% (%label_with_message% "not a number" label),

    PosNat
      | doc m%"
        Enforces that a number is a strictly positive natural number.

        # Examples

        ```nickel
        (42 | PosNat) =>
          42
        (0 | PosNat) =>
          error
        (-4 | PosNat) =>
          error
        ```
      "%
      = fun label value =>
        if %typeof% value == `Number then
          if value % 1 == 0 && value > 0 then
            value
          else
            %blame% (%label_with_message% "not positive integer" label)
        else
          %blame% (%label_with_message% "not a number" label),

    NonZero
      | doc m%"
        Enforces that a number is not equal to zero.

        # Examples

        ```nickel
        (1 | NonZero) =>
          1
        (0.0 | NonZero) =>
          error
        ```
      "%
      = fun label value =>
        if %typeof% value == `Number then
          if value != 0 then
            value
          else
            %blame% (%label_with_message% "non-zero" label)
        else
          %blame% (%label_with_message% "not a number" label),

    is_integer
      : Number -> Bool
      | doc m%"
        Checks if the given number is an integer.

        # Examples

        ```nickel
        is_int 42 =>
          true
        is_int 1.5 =>
          false
        ```
      "%
      = fun x => x % 1 == 0,

    min
      : Number -> Number -> Number
      | doc m%"
        Returns the lower of two numbers.

        # Examples

        ```nickel
        min (-1337) 42 =>
          -1337
        ```
      "%
      = fun x y => if x <= y then x else y,

    max
      : Number -> Number -> Number
      | doc m%"
        Returns the higher of two numbers.

        # Examples

        ```nickel
        max (-1337) 42 =>
          42
        ```
      "%
      = fun x y => if x >= y then x else y,

    floor
      : Number -> Number
      | doc m%"
        Rounds a number down to the next integer.

        # Examples

        ```nickel
        floor 42.5 =>
          42
        floor (-42.5) =>
          -43
        ```
      "%
      = fun x =>
        if x >= 0 then
          x - (x % 1)
        else
          x - 1 - (x % 1),

    abs
      : Number -> Number
      | doc m%"
        Returns the absolute value of a number.

        # Examples

        ```nickel
        abs (-5) =>
          5
        abs 42 =>
          42
        ```
      "%
      = fun x => if x < 0 then -x else x,

    fract
      : Number -> Number
      | doc m%"
        Returns the fractional part of a number.

        # Examples

        ```nickel
        fract 13.37 =>
          0.37
        fract 42 =>
          0
        ```
      "%
      = fun x => x % 1,

    truncate
      : Number -> Number
      | doc m%"
        Truncates a number, rounding it towards 0.

        # Examples

        ```nickel
        truncate (-13.37) =>
          -13
        truncate 42.5 =>
          42
        ```
      "%
      = fun x => x - (x % 1),

    pow
      : Number -> Number -> Number
      | doc m%"
        `pow x y` returns `x` to the power of `y`.

        # Examples

        ```nickel
        pow 2 8 =>
          256
        ```

        # Precision

        Nickel numbers are arbitrary precision rationals. If the exponent `y` is
        an integer which fits into a 64-bit signed or unsigned integer (that is, if
        `y` is an integer between `−2^63` and `2^64-1`), the result is computed
        exactly.

        Otherwise, both operands `x` and `y` are converted to the nearest 64 bit
        float (excluding `NaN` and infinity), and we compute the result as a 64
        bit float. This result is then converted back to a rational. In this
        case, **be aware that both the conversion from rationals to floats, and
        the power operation, might incur rounding errors**.
      "%
      = fun x n => %pow% x n,
  }
}
