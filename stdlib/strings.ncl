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

    NumLiteral = 
      let pattern = m#"^[+-]?(\d+(\.\d*)?(e[+-]?\d+)?|\.\d+(e[+-]?\d+)?)$"#m in
      fun l s =>
        if %isStr% s then
          if (%strMatch% s pattern).index > -1 then
            s
          else
            %blame% (%tag% "invalid num literal" l)
        else
          %blame% (%tag% "not a string" l);

    CharLiteral = fun l s =>
      if %isStr% s then
        if length s == 1 then
          s
        else
          %blame% (%tag% "length different than one" l)
      else
        %blame% (%tag% "not a string" l);

    Ident = fun l s =>
      if %isStr% s then
        //use regex here
        s 
      else
        %blame% (%tag% "not a string" l);

    NonEmpty = fun l s =>
      if %isStr% s then
        if %strLength% s > 0 then
          s
        else
          %blame% (%tag% "empty string" l)
      else
        %blame% (%tag% "not a string" l);

    // using a contract instead of type for now becaus of https://github.com/tweag/nickel/issues/226
    join | List Str -> Str -> Str = fun l sep =>
        lists.foldl (fun acc s => s ++ sep ++ acc) "" l;

    split : Str -> Str -> List Str = fun s sep => %strSplit% s sep;

    trim : Str -> Str = fun s => %strTrim% s;

    chars : Str -> List Str = fun s => %strChars% s;
    code | #CharLiteral -> Num = fun s => %charCode% s;
    fromCode | Num -> #CharLiteral = fun s => %charFromCode% s; 

    uppercase : Str -> Str = fun s => %strUppercase% s;
    lowercase : Str -> Str = fun s => %strLowercase% s;

    contains: Str -> Str -> Bool = fun s subs => %strContains% s subs;
    // Waiting for ternary primitive op
    // replace: Str -> Str -> Str -> Str = fun s pattern replace =>
    //   %strReplace% s pattern replace;

    match : Str -> Str -> {match: Str, index: Num, groups: List Str} =
     fun s regex => %strMatch% s regex;

    length : Str -> Num = fun s => %strLength% s;

    // Waiting for ternary primitive ops
    // substring: Str -> Num -> Num -> Str = fun s start end =>
    //  %strSubstring% s start end;

    fromNum | Num -> Str = fun n => %strFrom% n;
    // fromEnum | < | Dyn> -> Str = fun tag => %strFrom% tag;
    fromEnum | Dyn -> Str = fun tag => %strFrom% tag;
    fromBool | Bool -> Str = fun b => %strFrom% b;

    toNum | #NumLiteral -> Num = fun s => %numFrom% s;
    toBool | #BoolLiteral -> Bool = fun s => s == "true";
    // toEnum | #Ident -> < | Dyn> = fun s => %enumFrom% s;
    toEnum | #Ident -> Dyn = fun s => %enumFrom% s;
  }
}
