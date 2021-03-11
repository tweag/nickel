{
  nums = {
    Int = fun label value =>
      if %isNum% value then
        if value % 1 == 0 then
          value
        else
          %blame% (%tag% "not an integer" label)
      else
        %blame% (%tag% "not a number" label);

    Nat = fun label value =>
      if %isNum% value then
        if value % 1 == 0 && value >= 0 then
          value
        else
          %blame% (%tag% "not a natural" label)
      else
        %blame% (%tag% "not a number" label);

    PosNat = fun label value =>
      if %isNum% value then
        if value % 1 == 0 && value > 0 then
          value
        else
          %blame% (%tag% "not positive integer" label)
      else
        %blame% (%tag% "not a number" label);

    NonZero = fun label value =>
      if %isNum% value then
        if value != 0 then
          value
        else
          %blame% (%tag% "non-zero" label)
      else
        %blame% (%tag% "not a number" label);

    isInt : Num -> Bool = fun x =>
      %isNum% x && (x % 1 == 0);

    min : Num -> Num -> Num = fun x y =>
      if x <= y then x else y;

    max : Num -> Num -> Num = fun x y =>
      if x >= y then x else y;

    floor : Num -> Num = fun x =>
      if x >= 0 then x - (x % 1)
      else x - 1 - (x % 1);

    abs : Num -> Num = fun x =>
      if x < 0 then -x else x;

    fract : Num -> Num = fun x =>
      x % 1;

    trunc : Num -> Num = fun x =>
      x - (x % 1);

    pow : Num -> Num -> Num = fun x n =>
      %pow% x n;
  }
}
