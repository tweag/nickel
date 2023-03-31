{
  string = {
    BoolLiteral
      | doc m%"
        Enforces that the value is a string that represents a boolean
        literal, namely
        - "True" or "true", which will be normalized to "true"; or
        - "False" or "false", which will be normalized to "false".

        # Examples

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

    NumberLiteral
      | doc m%"
        Enforces that the value is a string that represents a numerical value.

        # Examples

        ```nickel
        ("+1.2" | NumberLiteral) =>
          "+1.2"
        ("5" | NumberLiteral) =>
          "5"
        (42 | NumberLiteral) =>
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
            %blame% (%label_with_message% "invalid number literal" l)
        else
          %blame% (%label_with_message% "not a string" l),

    Character
      | doc m%"
        Enforces that the value is a character, i.e. a string of length 1.

        # Examples

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

    Stringable
      | doc m%"
        Enforces that the value is convertible to a string via
        `builtin.to_string` or `string.from`. Accepted values are:

        - numbers
        - booleans
        - strings
        - enum tags
        - null

        # Examples

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
      = contract.from_predicate
        (
          fun value =>
            let type = builtin.typeof value in
            value == null
            || type == `Number
            || type == `Bool
            || type == `String
            || type == `Enum
        ),

    NonEmpty
      | doc m%"
        Enforces that the value is a non-empty string.

        # Examples

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

    join
      : String -> Array String -> String
      | doc m%"
        Joins an array of strings with a given separator.

        # Examples

        ```nickel
        join ", " [ "Hello", "World!" ] =>
          "Hello, World!"
        ```
      "%
      = fun sep fragments =>
        let length = %length% fragments in
        if length == 0 then
          ""
        else
          let first = %elem_at% fragments 0 in
          let rest =
            %array_slice% 1 length fragments
            |> array.fold_left (fun acc s => acc ++ sep ++ s) ""
          in
          first ++ rest,

    split
      : String -> String -> Array String
      | doc m%"
        Splits a string based on a separator string.

        Note that this function never splits up Unicode extended grapheme
        clusters, even in cases where the sought value exists within one.

        # Examples

        ```nickel
        split "," "1,2,3" =>
          [ "1", "2", "3" ]
        split "." "1,2,3" =>
          [ "1,2,3" ]
        ```
      "%
      = fun sep s => %str_split% s sep,

    trim
      : String -> String
      | doc m%"
        Trims whitespace from the start and end of a string.

        # Examples

        ```nickel
        trim " hi  " =>
          "hi"
        trim "1   2   3   " =>
          "1   2   3"
        ```
      "%
      = fun s => %str_trim% s,

    characters
      : String -> Array String
      | doc m%"
        Separates a string into its individual Unicode extended grapheme clusters.

        # Examples

        ```nickel
        chars "Hello" =>
          [ "H", "e", "l", "l", "o" ]
        ```
      "%
      = fun s => %str_chars% s,

    uppercase
      : String -> String
      | doc m%"
        Returns the uppercase version of the given character (including non-ascii characters) if it exists, the same
        character if not.

        # Examples

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

    lowercase
      : String -> String
      | doc m%"
        Returns the lowercase version of the given character (including non-ascii characters) if it exists, the same
        character if not.

        # Examples

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

    contains
      : String -> String -> Bool
      | doc m%"
        Checks if the first string is part of the second string.

        Note that this function returns false if the sought string exists entirely inside
        or straddles Unicode extended grapheme clusters.

        # Examples

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

    replace
      : String -> String -> String -> String
      | doc m%"
        `replace sub repl str` replaces every occurrence of `sub` in `str` with `repl`.

        Note that this function will not replace `sub` if it exists within a
        larger unicode extended grapheme cluster.

        # Examples

        ```nickel
        replace "cd" "   " "abcdef" =>
          "ab   ef"
        replace "" "A" "abcdef" =>
          "AaAbAcAdAeAfA"
        ```
      "%
      = fun pattern replace s =>
        %str_replace% s pattern replace,

    replace_regex
      : String -> String -> String -> String
      | doc m%"
        `replace_regex regex repl string` replaces every match of `regex` in `string` with `repl`.

        **WARNING**: this function will currently break up Unicode extended
        grapheme clusters if the regex matches a codepoint that exists within
        one. This behaviour is expected to change in a future version.

        # Examples

        ```nickel
        replace_regex "l+." "j" "Hello!" =>
          "Hej!"
        replace_regex "\\d+" "\"a\" is not" "This 37 is a number." =>
          "This \"a\" is not a number."
      ```
      "%
      = fun pattern replace s =>
        %str_replace_regex% s pattern replace,

    is_match
      : String -> String -> Bool
      | doc m%"
        `is_match regex string` checks if `string` matches `regex`.

        **WARNING**: this function will currently return `true` even if the
        regex match overlaps, or happens entirely within, with a Unicode extended
        grapheme cluster. This behaviour is expected to change in a future version.

        # Examples

        ```nickel
        is_match "^\\d+$" "123" =>
          true
        is_match "\\d{4}" "123" =>
          false
        ```

        # Performance

        When checking multiple strings against a common regular expression, it
        is advantageous to store a partially applied version of this function. This
        partial evaluation will store a compiled version of the regular expression and
        prevent recompilation at each call site.

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

    find
      : String -> String -> { matched : String, index : Number, groups : Array String }
      | doc m%"
        `find regex string` looks for matches of `regexp` in `string`. Returns
        the part of `string` that matched, the index of the first character that
        was part of the match in `string`, and an array of all capture groups if
        there were any.

        **WARNING**: this function will currently return incorrect indices if
        `str` contains Unicode extended grapheme clusters. The indices returned
        will be the location of the matching codepoints, rather than the
        matching grapheme clusters.

        # Examples

        ```nickel
        find "^(\\d).*(\\d).*(\\d).*$" "5 apples, 6 pears and 0 grapes" =>
          { matched = "5 apples, 6 pears and 0 grapes", index = 0, groups = [ "5", "6", "0" ] }
        find "3" "01234" =>
          { matched = "3", index = 3, groups = [ ] }
        ```

        # Performance

        Note that this function may perform better by sharing its partial application between multiple calls,
        because in this case the underlying regular expression will only be compiled once (See the documentation
        of `string.is_match` for more details).
      "%
      = fun regex => %str_find% regex,

    length
      : String -> Number
      | doc m%"
        Returns the length of the string, as measured by the number of Unicode
        [extended grapheme clusters](https://unicode.org/glossary/#extended_grapheme_cluster).

        Generally speaking, this gives the number of "visible" glyphs in the string.

        # Examples

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

    substring
      : Number -> Number -> String -> String
      | doc m%"
        `substring start end str` takes the slice of `str` from `start` (included) to `end` (excluded).

        The index arguments are the indices of Unicode extended grapheme clusters
        rather than codepoints.

        # Preconditions

        In `substring start end str`, `start` and `end` must be positive integers
        such that `0 <= start <= end <= array.length value`.

        # Examples

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

    from
      | Stringable -> String
      | doc m%"
        Converts a stringable value to its string representation. Same as
        `builtin.to_string`.

        # Examples

        ```nickel
        from 42 =>
          "42"
        from `Foo =>
          "Foo"
        from null =>
          "null"
        ```
      "%
      = fun x => %to_str% x,

    from_number
      | Number -> String
      | doc m%"
        Converts a number to its string representation.

        # Examples

        ```nickel
        from_number 42 =>
          "42"
        ```
      "%
      = from,

    from_enum
      | enum.Tag -> String
      | doc m%"
        Converts an enum variant to its string representation.

        # Examples

        ```nickel
        from_enum `MyEnum =>
          "MyEnum"
        ```
      "%
      = from,

    from_bool
      | Bool -> String
      | doc m%"
        Converts a boolean value to its string representation.

        # Examples

        ```nickel
        from_bool true =>
          "true"
        ```
      "%
      = from,

    to_number
      | NumberLiteral -> Number
      | doc m%"
        Converts a string that represents a number to that number.

        # Examples

        ```nickel
        to_number "123" =>
          123
        ```
      "%
      = fun s => %num_from_str% s,

    to_bool
      | BoolLiteral -> Bool
      | doc m%"
        Converts a string that represents a bool to that bool.

        # Examples

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

    to_enum
      | String -> enum.Tag
      | doc m%"
        Converts any string that represents an enum variant to that enum variant.

        # Examples

        ```nickel
        to_enum "Hello" =>
          `Hello
        ```
      "%
      = fun s => %enum_from_str% s,
  }
}
