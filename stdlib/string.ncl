{
  string = {
    BoolLiteral
    | doc m%"
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
      "%
    = fun l s =>
      if %typeof% s == `String then
        if s == "true" || s == "True" then
          "true"
        else if s == "false" || s == "False" then
          "false"
        else
          %blame% (%label_with_message% "expected \"true\" or \"false\", got %{s}" l)
      else
        %blame% (%label_with_message% "not a string" l),

    NumLiteral
    | doc m%"
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
      "%
    = let pattern = m%"^[+-]?(\d+(\.\d*)?(e[+-]?\d+)?|\.\d+(e[+-]?\d+)?)$"% in
      let is_num_literal = %str_is_match% pattern in
      fun l s =>
        if %typeof% s == `String then
          if is_num_literal s then
            s
          else
            %blame% (%label_with_message% "invalid num literal" l)
        else
          %blame% (%label_with_message% "not a string" l),

    Character
    | doc m%"
      Contract to enforce the value is a character (i.e. a string of length 1).

      For example:
      ```nickel
        ("e" | Character) =>
          "e"
        ("#" | Character) =>
          "#"
        ("" | Character) =>
          error
        (1 | Character) =>
          error
      ```
      "%
    = fun l s =>
      if %typeof% s == `String then
        if length s == 1 then
          s
        else
          %blame% (%label_with_message% "length different than one" l)
      else
        %blame% (%label_with_message% "not a string" l),

    EnumTag
    | doc m%"
      Contract to enforce the value is an enum tag.

      For example:
      ```nickel
        (`foo | EnumTag) =>
          `foo
        (`FooBar | EnumTag) =>
          `FooBar
        ("tag" | EnumTag) =>
          error
      ```
      "%
    = contract.from_predicate builtin.is_enum,

    Stringable
    | doc m%"
      Contract to enforce the value is convertible to a string via
      `builtin.to_string` or `string.from`. Accepted values are:

      - numbers
      - booleans
      - strings
      - enum tags
      - null

      For example:
      ```nickel
        (`Foo | Stringable) =>
          `Foo
        (false | Stringable) =>
          false
        ("bar" ++ "foo" | Stringable) =>
          "barfoo"
        ({foo = "baz"} | Stringable) =>
          error
      ```
      "%
    = contract.from_predicate (fun value =>
      let type = builtin.typeof value in
      value == null
      || type == `Number
      || type == `Bool
      || type == `String
      || type == `Enum),

    NonEmpty
    | doc m%"
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
      "%
    = fun l s =>
      if %typeof% s == `String then
        if %str_length% s > 0 then
          s
        else
          %blame% (%label_with_message% "empty string" l)
      else
        %blame% (%label_with_message% "not a string" l),

    join : String -> Array String -> String
    | doc m%"
      Joins a array of strings given a separator.

      For example:
      ```nickel
        join ", " [ "Hello", "World!" ] =>
          "Hello, World!"
      ```
      "%
    = fun sep l =>
      if %length% l == 0 then
        ""
      else
        %head% l ++ array.fold_left (fun acc s => acc ++ sep ++ s) "" (%tail% l),

    split : String -> String -> Array String
    | doc m%"
      Splits a string based on a separator string. The separator string is not included in any string.

      For example:
      ```nickel
      split "," "1,2,3" =>
        [ "1", "2", "3" ]
      split "." "1,2,3" =>
        [ "1,2,3" ]
      ```
      "%
    = fun sep s => %str_split% s sep,

    trim : String -> String
    | doc m%"
      Trims whitespace from the start and end of the string.

      For example:
      ```nickel
      trim " hi  " =>
        "hi"
      trim "1   2   3   " =>
        "1   2   3"
      ```
      "%
    = fun s => %str_trim% s,

    characters : String -> Array String
    | doc m%"
      Separates a string into its individual characters.

      For example:
      ```nickel
        chars "Hello" =>
          [ "H", "e", "l", "l", "o" ]
      ```
      "%
    = fun s => %str_chars% s,

    codepoint | Character -> Number
    | doc m%%"
      Results in the unicode codepoint of the given character if it fits into a single codepoint.

      For example:
      ```nickel
        code "A" =>
          65
        code "%" =>
          37
        code "å" =>
          error
      ```
      "%%
    = fun s => %char_code% s,

    from_codepoint | Number -> Character
    | doc m%%"
      Results in the character for a given unicode codepoint.

      For example:
      ```nickel
        from_code 65 =>
          "A"
        from_code 37 =>
          "%"
        from_code 128 =>
          error
      ```
      "%%
    = fun s => %char_from_code% s,

    uppercase : String -> String
    | doc m%"
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
      "%
    = fun s => %str_uppercase% s,

    lowercase : String -> String
    | doc m%"
      Results in the lowercase version of the given character (including non-ascii characters) if it exists, the same
      character if not.

      For example:
      ```nickel
        lowercase "A" =>
          "a"
        lowercase "Æ" =>
          "æ"
        lowercase "." =>
          "."
      ```
      "%
    = fun s => %str_lowercase% s,

    contains: String -> String -> Bool
    | doc m%"
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
      "%
    = fun subs s => %str_contains% s subs,

    replace: String -> String -> String -> String
    | doc m%"
      `replace sub repl String` replaces every occurrence of `sub` in `String` with `repl`.

      For example:
      ```nickel
        replace "cd" "   " "abcdef" =>
          "ab   ef"
        replace "" "A" "abcdef" =>
          "AaAbAcAdAeAfA"
      ```
      "%
    = fun pattern replace s =>
       %str_replace% s pattern replace,

    replace_regex: String -> String -> String -> String
    | doc m%"
      `replace_regex regex repl String` replaces every match of `regex` in `String` with `repl`.

      For example:
      ```nickel
        replace_regex "l+." "j" "Hello!" =>
          "Hej!"
        replace_regex "\\d+" "\"a\" is not" "This 37 is a number." =>
          "This \"a\" is not a number."
      ```
      "%
    = fun pattern replace s =>
       %str_replace_regex% s pattern replace,

    is_match : String -> String -> Bool
    | doc m%"
      `is_match regex String` checks if `String` matches `regex`.

      For example:
      ```nickel
        is_match "^\\d+$" "123" =>
          true
        is_match "\\d{4}" "123" =>
          false
      ```

      For example, in the following program, the whole call to
      `string.is_match "[0-9]*\\.?[0-9]+ x"` is re-evaluated at each invocation of
      `is_number`. The regexp will be recompiled 3 times in total:

      ```nickel
      let is_number = fun x => string.is_match "[0-9]*\\.?[0-9]+" x in
      ["0", "42", "0.5"] |> array.all is_number =>
        true
      ```

      On the other hand, in the version below, the partial application of
      `string.is_match "[0-9]*\\.?[0-9]+"` is evaluated once, returning a
      function capturing the compiled regexp. The regexp will only be compiled
      once and for all:

      ```nickel
      let is_number' = string.is_match "[0-9]*\\.?[0-9]+" in
      ["0", "42", "0.5"] |> array.all is_number' =>
        true
      ```
      "%
    = fun regex => %str_is_match% regex,

    find : String -> String -> {matched: String, index: Number, groups: Array String}
    | doc m%"
      `find regex String` matches `String` given `regex`. Results in the part of `String` that matched, the index of the
      first character that was part of the match in `String`, and a arrays of all capture groups if any.

      For example:
      ```nickel
        find "^(\\d).*(\\d).*(\\d).*$" "5 apples, 6 pears and 0 grapes" =>
          { matched = "5 apples, 6 pears and 0 grapes", index = 0, groups = [ "5", "6", "0" ] }
        find "3" "01234" =>
          { matched = "3", index = 3, groups = [ ] }
      ```

      Note that this function may perform better by sharing its partial application between multiple calls,
      because in this case the underlying regular expression will only be compiled once (See the documentation
      of `string.is_match` for more details).
      "%
    = fun regex => %str_find% regex,

    length : String -> Number
    | doc m%"
      Returns the length of the string, as measured by the number of UTF-8
      [extended grapheme clusters](https://unicode.org/glossary/#extended_grapheme_cluster).

      Generally speaking, this gives the number of "visible" glyphs in the string.

      For example:
      ```nickel
        length "" =>
          0
        length "hi" =>
          2
        length "四字熟語" =>
          4
        length "👨🏾‍❤️‍💋‍👨🏻" =>
          1
      ```
      "%
    = fun s => %str_length% s,

    substring: Number -> Number -> String -> String
    | doc m%"
      Takes a slice from the string. Errors if either index is out of range.

      For example:
      ```nickel
        substring 3 5 "abcdef" =>
          "de"
        substring 3 10 "abcdef" =>
          error
        substring (-3) 4 "abcdef" =>
          error
      ```
      "%
    = fun start end s => %str_substr% s start end,

    from | Stringable -> String
    | doc m%"
      Converts a correct value to a string representation. Same as
      `builtin.to_string`.

      For example:
      ```nickel
      from 42 =>
        "42"
      from `Foo =>
        "Foo"
      from null =>
        "null"
      "%
    = fun x => %to_str% x,

    from_num | Number -> String
    | doc m%"
      Converts a number to its string representation.

      For example:
      ```nickel
      from_num 42 =>
        "42"
      ```
      "%
    = from,

    # from_enum | < | Dyn> -> String = fun tag => %to_str% tag,
    from_enum | EnumTag -> String
    | doc m%"
      Converts an enum variant to its string representation.

      For example:
      ```nickel
      from_enum `MyEnum =>
        "MyEnum"
      ```
      "%
    = from,

    from_bool | Bool -> String
    | doc m%"
      Converts a boolean value to its string representation.

      For example:
      ```nickel
        from_bool true =>
          "true"
      ```
      "%
    = from,

    to_num | NumLiteral -> Number
    | doc m%"
      Converts a string that represents an integer to that integer.

      For example:
      ```nickel
        to_num "123" =>
          123
      ```
      "%
    = fun s => %num_from_str% s,

    to_bool | BoolLiteral -> Bool
    | doc m%"
      Converts a string that represents a boolean to that boolean.

      For example:
      ```nickel
        to_bool "true" =>
          true
        to_bool "True" =>
          true
        to_bool "false" =>
          false
      ```
      "%
    = fun s => s == "true",

    # to_enum | String -> < | Dyn> = fun s => %enum_from_str% s,
    to_enum | String -> EnumTag
    | doc m%"
      Converts any string that represents an enum variant to that enum variant.

      For example:
      ```nickel
        to_enum "Hello" =>
          `Hello
      ```
      "%
    = fun s => %enum_from_str% s,
  }
}
