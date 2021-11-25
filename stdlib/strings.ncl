{
  strings = {
    BoolLiteral
    | doc m#"
      Contract to enforce the value is a string that represents a boolean literal. Additionally casts "True" to "true"
      and "False" to "false".

      For example:
      ```nickel
        ("True" | BoolLiteral) =>
          "true"
        ("hello" | BoolLiteral) =>
          error
        (true | BoolLiteral) =>
          error
      ```
      "#m
    = fun l s =>
      if %isStr% s then
        if s == "true" || s == "True" then
          "true"
        else if s == "false" || s == "False" then
          "false"
        else
          %blame% (%tag% "expected \"true\" or \"false\", got #{s}" l)
      else
        %blame% (%tag% "not a string" l),

    NumLiteral
    | doc m#"
      Contract to enforce the value is a string that represends a numerical value.

      For example:
      ```nickel
        ("+1.2" | NumLiteral) =>
          "+1.2"
        ("5" | NumLiteral) =>
          "5"
        (42 | NumLiteral) =>
          error
      ```
      "#m
    = let pattern = m#"^[+-]?(\d+(\.\d*)?(e[+-]?\d+)?|\.\d+(e[+-]?\d+)?)$"#m in
      fun l s =>
        if %isStr% s then
          if %strIsMatch% s pattern then
            s
          else
            %blame% (%tag% "invalid num literal" l)
        else
          %blame% (%tag% "not a string" l),

    CharLiteral
    | doc m#"
      Contract to enforce the value is a character literal (i.e. a string of length 1).

      For example:
      ```nickel
        ("e" | CharLiteral) =>
          "e"
        ("#" | CharLiteral) =>
          "#"
        ("" | CharLiteral) =>
          error
        (1 | CharLiteral) =>
          error
      ```
      "#m
    = fun l s =>
      if %isStr% s then
        if length s == 1 then
          s
        else
          %blame% (%tag% "length different than one" l)
      else
        %blame% (%tag% "not a string" l),

    Ident
    | doc m#"
      Contract to enforce the value is a valid Nickel identifier. Can be used in conjunction with the record field
      function (e.g. `hasField`).

      For example:
      ```nickel
        records.hasField ("hello" | Ident) { hello = "hi!" } =>
          true
        records.hasField ("9" | Ident) { hello = "hi!" } =>
          error (invalid enum tag)
      "#m
    = fun l s =>
      let pattern = m#"_?[a-zA-Z][_a-zA-Z0-9]*"#m in
      if %isStr% s then
        if %strIsMatch% s pattern then
            s
          else
            %blame% (%tag% "invalid enum tag" l)
      else
        %blame% (%tag% "not a string" l),

    NonEmpty
    | doc m#"
      Contract to enforce the value is a non-empty string.

      For example:
      ```nickel
        ("" | NonEmpty) =>
          error
        ("hi!" | NonEmpty) =>
          "hi!"
        (42 | NonEmpty) =>
          error
      ```
      "#m
    = fun l s =>
      if %isStr% s then
        if %strLength% s > 0 then
          s
        else
          %blame% (%tag% "empty string" l)
      else
        %blame% (%tag% "not a string" l),

    // using a contract instead of type for now because of https://github.com/tweag/nickel/issues/226
    join | Str -> List Str -> Str
    | doc m#"
      Joins a list of strings given a seperator.

      For example:
      ```nickel
        join ", " [ "Hello", "World!" ] =>
          "Hello, World!"
      ```
      "#m
    = fun sep l =>
      if %length% l == 0 then
        ""
      else
        %head% l ++ lists.foldl (fun acc s => acc ++ sep ++ s) "" (%tail% l),

    split : Str -> Str -> List Str
    | doc m#"
      Splits a string based on a separator string. The separator string is not included in any string.

      For example:
      ```nickel
      split "1,2,3" "," =>
        [ "1", "2", "3" ]
      split "1,2,3" "." =>
        [ "1,2,3" ]
      ```
      "#m
    = fun s sep => %strSplit% s sep,

    trim : Str -> Str
    | doc m#"
      Trims whitespace from the start and end of the string.

      For example:
      ```nickel
      trim " hi  " =>
        "hi"
      trim "1   2   3   " =>
        "1   2   3"
      ```
      "#m
    = fun s => %strTrim% s,

    chars : Str -> List Str
    | doc m#"
      Separates a string into its individual characters.

      For example:
      ```nickel
        chars "Hello" =>
          [ "H", "e", "l", "l", "o" ]
      ```
      "#m
    = fun s => %strChars% s,

    code | #CharLiteral -> Num
    | doc m#"
      Results in the ascii code of the given character.

      For example:
      ```nickel
        code "A" =>
          65
        code "%" =>
          37
        code "å" =>
          error
      ```
      "#m
    = fun s => %charCode% s,

    fromCode | Num -> #CharLiteral
    | doc m#"
      Results in the character for a given ascii code. Any number outside the ascii range results in an error.

      For example:
      ```nickel
        fromCode 65 =>
          "A"
        fromCode 37 =>
          "%"
        fromCode 128 =>
          error
      ```
      "#m
    = fun s => %charFromCode% s,

    uppercase : Str -> Str
    | doc m#"
      Results in the uppercase version of the given character (including non-ascii characters) if it exists, the same
      character if not.

      For example:
      ```nickel
        uppercase "a" =>
          "A"
        uppercase "æ" =>
          "Æ"
        uppercase "." =>
          "."
      ```
      "#m
    = fun s => %strUppercase% s,

    lowercase : Str -> Str
    | doc m#"
      Results in the lowercase version of the given character (including non-ascii characters) if it exists, the same
      character if not.

      For example:
      ```nickel
        uppercase "A" =>
          "a"
        uppercase "Æ" =>
          "æ"
        uppercase "." =>
          "."
      ```
      "#m
    = fun s => %strLowercase% s,

    contains: Str -> Str -> Bool
    | doc m#"
      Checks if the first string is part of the second string.

      For example:
      ```nickel
        contains "cde" "abcdef" =>
          true
        contains "" "abcdef" =>
          true
        contains "ghj" "abcdef" =>
          false
      ```
      "#m
    = fun subs s => %strContains% s subs,

    replace: Str -> Str -> Str -> Str
    | doc m#"
      `replace str sub repl` replaces every occurence of `sub` in `str` with `repl`.

      For example:
      ```nickel
        replace "abcdef" "cd" "   " =>
          "ab   ef"
        replace "abcdef" "" "A" =>
          "AaAbAcAdAeAfA"
      ```
      "#m
    = fun s pattern replace =>
       %strReplace% s pattern replace,

    replaceRegex: Str -> Str -> Str -> Str
    | doc m#"
      `replaceRegex str regex repl` replaces every match of `regex` in `str` with `repl`.

      For example:
      ```nickel
        replaceRegex "Hello!" "l+." "j" =>
          "Hej!"
        replaceRegex "This 37 is a number." "\\d+" "\"a\" is not" =>
          "This "a" is not a number."
      ```
      "#m
    = fun s pattern replace =>
       %strReplaceRegex% s pattern replace,

    isMatch : Str -> Str -> Bool
    | doc m#"
      `isMatch regex str` checks if `str` matches `regex`.

      For example:
      ```nickel
        isMatch "^\\d+$" "123" =>
          true
        isMatch "\\d{4}" "123" =>
          false
      ```
      "#m
    = fun regex s => %strIsMatch% s regex,

    match : Str -> Str -> {match: Str, index: Num, groups: List Str}
    | doc m#"
      `match str regex` matches `str` given `regex`. Results in the part of `str` that matched, the index of the
      first character that was part of the match in `str`, and a lists of all capture groups if any.

      For example:
      ```nickel
        match "5 apples, 6 pears and 0 grapes" "^(\\d).*(\\d).*(\\d).*$" =>
          { match = "5 apples, 6 pears and 0 grapes", index = 0, groups = [ "5", "6", "0" ] }
        match "01234" "3" =>
          { match = "3", index = 3, groups = [ ] }
      ```
      "#m
    = fun s regex => %strMatch% s regex,

    length : Str -> Num
    | doc m#"
      Results in the length of the given string.

      For example:
      ```nickel
        length "" =>
          0
        length "hi" =>
          2
      ```
      "#m
    = fun s => %strLength% s,

    substring: Str -> Num -> Num -> Str
    | doc m#"
      Takes a slice from the string. Errors if either index is out of range.

      For example:
      ```nickel
        substring "abcdef" 3 5 =>
          "de"
        substring "abcdef" 3 10 =>
          error
        substring "abcdef" (-3) 4 =>
          error
      ```
      "#m
    = fun s start end => %strSubstr% s start end,

    fromNum | Num -> Str
    | doc m#"
      Converts a number to its string representation.

      fromNum 42 =>
        "42"
      "#m
    = fun n => %toStr% n,

    // fromEnum | < | Dyn> -> Str = fun tag => %toStr% tag,
    fromEnum | Dyn -> Str
    | doc m#"
      Converts an enum variant to its string representation.

      For example:
      ```nickel
      fromEnum `MyEnum =>
        "MyEnum"
      ```
      "#m
    = fun tag => %toStr% tag,

    fromBool | Bool -> Str
    | doc m#"
      Converts a boolean value to its string representation.

      For example:
      ```nickel
        fromBool true =>
          "true"
      ```
      "#m
    = fun b => %toStr% b,

    toNum | #NumLiteral -> Num
    | doc m#"
      Converts a string that represents an integer to that integer.

      For example:
      ```nickel
        toNum "123" =>
          123
      ```
      "#m
    = fun s => %numFromStr% s,

    toBool | #BoolLiteral -> Bool
    | doc m#"
      Converts a string that represents a boolean to that boolean.

      For example:
      ```nickel
        toBool "true" =>
          true
        toBool "True" =>
          true
        toBool "false" =>
          false
      "#m
    = fun s => s == "true",

    // toEnum | #Ident -> < | Dyn> = fun s => %enumFromStr% s,
    toEnum | #Ident -> Dyn
    | doc m#"
      Converts any string that represents an enum variant to that enum variant.

      For example:
      ```nickel
        toEnum "Hello" =>
          `Hello
      ```
      "#m
    = fun s => %enumFromStr% s,
  }
}
