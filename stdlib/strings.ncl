{
  strings =
  {
    BoolLiteral = fun s l =>
      if %isStr% s then
        if s == "true" || s == "True" then
          "true"
        else if s == "false" || s == "False" then
          "false"
        else
          %blame% (%tag% "expected \"true\" or \"false\", got #{s} l)
      else
        %blame% (%tag% "expected string" l);

    NumLiteral = fun s l =>
      if %isStr% s then
        // use regex here
        if true then
          s
        else
          %blame% (%tag% "expected \"true\" or \"false\", got #{s} l)
      else
        %blame% (%tag% "not a string" l);

    CharLiteral = fun

    Ident = fun s l =>
      if %isStr% s then
        //use regex here
        if true then
      else
        %blame% (%tag% "not a string" l);

    NonEmpty = fun s l =>
      if %isStr% then
        if %strLength% s != 0 then
          s
        else
          %blame% (%tag% "empty string" l)
      else
        %blame% (%tag% "not a string" l);

    join = fun l sep =>
      if l == [] then
        ""
      else
        lists.foldl (fun acc s => acc ++ sep ++ s) (%head% l) (%tail% l);

    split : Str -> Str -> List Str = fun s sep => %split% s sep;

    trim : Str -> Str = fun s => %trim% s;

    chars : Str -> List Str => fun s => %strChars% s;
    code : #NonEmpty -> Num => fun s => %charCode% s;

    uppercase : Str -> Str = fun s => %uppercase% s;
    lowercase : Str -> Str = fun s => %lowercase% s;
    contains: Str -> Str -> Bool = fun s needle => %contains% s needle;
    replace: Str -> Str -> Str -> Str = fun s pattern replace =>
      %replace% s pattern replace;

    match : Str -> Str -> { | Dyn } = %blame% "fail";

    length : Str -> Num = fun s => %strLength% s;
    substring: Str -> Num -> Num -> Str = fun s start end =>
      %substring% s start end;

    fromNum : Num -> Str = fun n => %strFrom% n;
    fromEnum : forall a. < | a> -> Str => fun enum => %strFrom% enum;
    fromBool : Bool -> Str => fun bool => %strFrom% bool;

    toNum | #NumLiteral -> Num = fun s => %numFrom% n;
    toBool | #BoolLiteral -> Bool = fun s => s == "true";
    toEnum | #Ident -> Bool = fun s => %enumFrom% s;
  }
}
