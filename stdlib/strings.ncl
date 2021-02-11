{
  strings =
  {
    BoolLiteral = fun l s =>
      if %isStr% s then
        if s == "true" || s == "True" then
          "true"
        else if s == "false" || s == "False" then
          "false"
        else
          %blame% (%tag% "expected \"true\" or \"false\", got #{s}" l)
      else
        %blame% (%tag% "not a string" l);

    NumLiteral = fun l s =>
      if %isStr% s then
        // use regex here
        if true then
          s
        else
          %blame% l
      else
        %blame% (%tag% "not a string" l);

    CharLiteral = fun l s =>
      if %isStr% s then
        if length s == 1 then
          s
        else
          %blame% (%tag% "longer or shorter than 1")
      else
        %blame (%tag% "not a string" l);

    Ident = fun l s =>
      if %isStr% s then
        //use regex here
        if true then
      else
        %blame% (%tag% "not a string" l);

    NonEmpty = fun l s =>
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

    split : Str -> Str -> List Str = fun s sep => %strSplit% s sep;

    trim : Str -> Str = fun s => %strTrim% s;

    chars : Str -> List Str => fun s => %strChars% s;
    code : #CharLiteral -> Num => fun s => %charCode% s;
    fromCode : Num -> #CharLiteral => fun s => %charFromCode% s; 

    uppercase : Str -> Str = fun s => %strUppercase% s;
    lowercase : Str -> Str = fun s => %strLowercase% s;

    contains: Str -> Str -> Bool = fun s subs => %strContains% s subs;
    // Waiting for ternary primitive op
    // replace: Str -> Str -> Str -> Str = fun s pattern replace =>
    //   %strReplace% s pattern replace;

    match : Str -> Str -> {match: Str, index: Num, groups: List Str} = %blame% "fail";

    length : Str -> Num = fun s => %strLength% s;

    // Waiting for ternary primitive ops
    // substring: Str -> Num -> Num -> Str = fun s start end =>
    //  %strSubstring% s start end;

    fromNum | Num -> Str = fun n => %toStr% n;
    fromEnum | forall a. < | a> -> Str => fun tag => %toStr% tag;
    fromBool | Bool -> Str => fun b => %toStr% b;

    toNum | #NumLiteral -> Num = fun s => %numFrom% n;
    toBool | #BoolLiteral -> Bool = fun s => s == "true";
    toEnum | #Ident -> < | Dyn> = fun s => %enumFrom% s;
  }
}
