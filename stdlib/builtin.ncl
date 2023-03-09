{
  builtin = {
    is_number : Dyn -> Bool
    | doc m%"
      Checks if the given value is a number.

      For example:
      ```nickel
        is_number 1 =>
          true
        is_number "Hello, World!" =>
          false
      ```
      "%
    = fun x => %typeof% x == `Number,

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

    is_string : Dyn -> Bool
    | doc m%"
      Checks if the given value is a string.

      For example:
      ```nickel
        is_string true =>
          false
        is_string "Hello, World!" =>
          true
      ```
      "%
    = fun x => %typeof% x == `String,

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

    is_function : Dyn -> Bool
    | doc m%"
      Checks if the given value is a function.

      For example
      ```nickel
        is_function (fun x => x) =>
          true
        is_function 42 =>
          false
      ```
      "%
    = fun x => %typeof% x == `Function,

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
        `Number,
        `Bool,
        `String,
        `Enum,
        `Label,
        `Function,
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
          `Function
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

    hash : [| `Md5, `Sha1, `Sha256, `Sha512 |] -> String -> String
    | doc m%"
      Hashes the given string provided the desired hash algorithm.

      For example:
      ```nickel
        hash `Md5 "hunter2" =>
          "2ab96390c7dbe3439de74d0c9b0b1767"
      ```
      "%
    = fun type s => %hash% type s,

    serialize : [| `Json, `Toml, `Yaml |] -> Dyn -> String
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

    deserialize : [| `Json, `Toml, `Yaml |] -> String -> Dyn
    | doc m%"
      Deserializes the given string to a nickel value given the encoding of the string.

      For example:
      ```nickel
        deserialize `Json "{ \"hello\": \"Hello\", \"world\": \"World\" }"
          { hello = "Hello", world = "World" }
      ```
      "%
    = fun format x => %deserialize% format x,

    to_string | string.Stringingable -> String
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
      ```
      "%
    = fun x => %to_str% x,

    trace : forall a. String -> a -> a
    | doc m%"
      `builtin.trace msg x` prints `msg` to standard output when encountered by the evaluator,
      and proceed with the evaluation of `x`.

      For example:
      ```nickel
      builtin.trace "Hello, world!" true =>
        builtin.trace: Hello, world!
        true
      ```
      "%
    = fun msg x => %trace% msg x,

    FailWith
    | doc m%"
      A contract that always fails with a given message.

      For example:
      ```nickel
      1 | FailWith "message" =>
        error: contract broken by a value: message
          ┌─ :1:1
          │
        1 │ FailWith "message"
          │ ---------------------------- expected type
          │
          ┌─ repl-input-0:1:1
          │
        1 │ 1 | FailWith "message"
          │ ^ applied to this expression
      ```
    "%
    = fun msg label value => contract.blame_with_message msg label,

    fail_with | String -> Dyn
    | doc m%"
      Unconditionally abort evaluation with the given message.

      For example:
      ```nickel
      fail_with "message" =>
        error: contract broken by a value: message
      ```
    "%
    = fun msg => null | FailWith msg,

  },
}
