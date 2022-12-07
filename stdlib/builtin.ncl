{
  builtin = {
    is_num : Dyn -> Bool
    | doc m%"
      Checks if the given value is a number.

      For example:
      ```nickel
        is_num 1 =>
          true
        is_num "Hello, World!" =>
          false
      ```
      "%
    = fun x => %typeof% x == `Num,

    is_bool : Dyn -> Bool
    | doc m%"
      Checks if the given value is a boolean.

      For example:
      ```nickel
        is_bool false =>
          true
        is_bool 42 =>
          false
      ```
      "%
    = fun x => %typeof% x == `Bool,

    is_str : Dyn -> Bool
    | doc m%"
      Checks if the given value is a string.

      For example:
      ```nickel
        is_str true =>
          false
        is_str "Hello, World!" =>
          true
      ```
      "%
    = fun x => %typeof% x == `Str,

    is_enum : Dyn -> Bool
    | doc m%"
      Checks if the given value is an enum tag.

      For example:
      ```nickel
        is_enum true =>
          false
        is_enum `false =>
          true
      ```
      "%

    = fun x => %typeof% x == `Enum,

    is_fun : Dyn -> Bool
    | doc m%"
      Checks if the given value is a function.

      For example
      ```nickel
        is_fun (fun x => x) =>
          true
        is_fun 42 =>
          false
      ```
      "%
    = fun x => %typeof% x == `Fun,

    is_array : Dyn -> Bool
    | doc m%"
      Checks if the given value is a array.

      For example
      ```nickel
        is_array [ 1, 2 ] =>
          true
        is_array 42 =>
          false
      ```
      "%
    = fun x => %typeof% x == `Array,

    is_record : Dyn -> Bool
    | doc m%"
      Checks if the given value is a record.

      For example
      ```nickel
        is_record [ 1, 2 ] =>
          false
        is_record { hello = "Hello", world = "World" } =>
          true
      ```
      "%
    = fun x => %typeof% x == `Record,

    typeof : Dyn -> [|
        `Num,
        `Bool,
        `Str,
        `Enum,
        `Lbl,
        `Fun,
        `Array,
        `Record,
        `Other
      |]
    | doc m%"
      Results in a value representing the type of the typed value.

      For example:
      ```nickel
        typeof [ 1, 2 ] =>
          `Array
        typeof (fun x => x) =>
          `Fun
      ```
      "%
    = fun x => %typeof% x,

    seq : forall a. Dyn -> a -> a
    | doc m%"
      `seq x y` forces the evaluation of `x`, before resulting in `y`.

      For example:
      ```nickel
        seq (42 / 0) 37 =>
          error
        seq (42 / 2) 37 =>
          37
        seq { tooFar = 42 / 0 } 37 =>
          37
      ```
      "%
    = fun x y => %seq% x y,

    deep_seq : forall a. Dyn -> a -> a
    | doc m%"
      `deep_seq x y` forces a deep evaluation `x`, before resulting in `y`.

      For example:
      ```nickel
        deep_seq (42 / 0) 37 =>
          error
        deep_seq (42 / 2) 37 =>
          37
        deep_seq { tooFar = 42 / 0 } 37 =>
          error
      ```
      "%
    = fun x y => %deep_seq% x y,

    hash : [| `Md5, `Sha1, `Sha256, `Sha512 |] -> Str -> Str
    | doc m%"
      Hashes the given string provided the desired hash algorithm.

      For example:
      ```nickel
        hash `Md5 "hunter2" =>
          "2ab96390c7dbe3439de74d0c9b0b1767"
      ```
      "%
    = fun type s => %hash% type s,

    serialize : [| `Json, `Toml, `Yaml |] -> Dyn -> Str
    | doc m%"
      Serializes the given value to the desired representation.

      For example:
      ```nickel
        serialize `Json { hello = "Hello", world = "World" } =>
          "{
            "hello": "Hello",
            "world": "World"
          }"
      ```
      "%
    = fun format x => %serialize% format (%force% x),

    deserialize : [| `Json, `Toml, `Yaml |] -> Str -> Dyn
    | doc m%"
      Deserializes the given string to a nickel value given the encoding of the string.

      For example:
      ```nickel
        deserialize `Json "{ \"hello\": \"Hello\", \"world\": \"World\" }"
          { hello = "Hello", world = "World" }
      ```
      "%
    = fun format x => %deserialize% format x,

    to_str | string.Stringable -> Str
    | doc m%"
      Converts a stringable value to a string representation. Same as
      `string.from`.

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

  },
}
