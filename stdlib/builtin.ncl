{
  builtin = {
    is_number
      : Dyn -> Bool
      | doc m%"
        Checks if a value is a number.

        # Examples
        
        ```nickel
        is_number 1 =>
          true
        is_number "Hello, World!" =>
          false
        ```
      "%
      = fun x => %typeof% x == `Number,

    is_bool
      : Dyn -> Bool
      | doc m%"
        Checks if a value is a bool.

        # Examples
        
        ```nickel
        is_bool false =>
          true
        is_bool 42 =>
          false
        ```
      "%
      = fun x => %typeof% x == `Bool,

    is_string
      : Dyn -> Bool
      | doc m%"
        Checks if a value is a string.

        # Examples
        
        ```nickel
        is_string true =>
          false
        is_string "Hello, World!" =>
          true
        ```
      "%
      = fun x => %typeof% x == `String,

    is_enum
      : Dyn -> Bool
      | doc m%"
        Checks if a value is an enum tag.

        # Examples
        
        ```nickel
        is_enum true =>
          false
        is_enum `false =>
          true
        ```
      "%
      = fun x => %typeof% x == `Enum,

    is_function
      : Dyn -> Bool
      | doc m%"
        Checks if a value is a function.

        # Examples
        
        ```nickel
        is_function (fun x => x) =>
          true
        is_function 42 =>
          false
        ```
      "%
      = fun x => %typeof% x == `Function,

    is_array
      : Dyn -> Bool
      | doc m%"
        Checks if a value is an array.

        # Examples
        
        ```nickel
        is_array [ 1, 2 ] =>
          true
        is_array 42 =>
          false
        ```
      "%
      = fun x => %typeof% x == `Array,

    is_record
      : Dyn -> Bool
      | doc m%"
      Checks if the given value is a record.
        Checks if a value is a record.

        # Examples
        
        ```nickel
        is_record [ 1, 2 ] =>
          false
        is_record { hello = "Hello", world = "World" } =>
          true
        ```
      "%
      = fun x => %typeof% x == `Record,

    typeof
      : Dyn -> [|
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
        Returns the type of a value.

        # Examples
        
        ```nickel
        typeof [ 1, 2 ] =>
          `Array
        typeof (fun x => x) =>
          `Function
        ```
      "%
      = fun x => %typeof% x,

    seq
      : forall a. Dyn -> a -> a
      | doc m%"
        `seq x y` forces the evaluation of `x`, before returning `y`.

        # Examples
        
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

    deep_seq
      : forall a. Dyn -> a -> a
      | doc m%"
        `deep_seq x y` forces a deep evaluation `x`, before returning `y`.

        # Examples
        
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

    hash
      : [| `Md5, `Sha1, `Sha256, `Sha512 |] -> String -> String
      | doc m%"
        Hashes the given string with the desired hashing algorithm.

        # Examples
        
        ```nickel
        hash `Md5 "hunter2" =>
          "2ab96390c7dbe3439de74d0c9b0b1767"
        ```
      "%
      = fun type s => %hash% type s,

    serialize
      : [| `Json, `Toml, `Yaml |] -> Dyn -> String
      | doc m%"
        Serializes a value into the desired representation.

        # Examples
        
        ```nickel
        serialize `Json { hello = "Hello", world = "World" } =>
          "{
            \"hello\": \"Hello\",
            \"world\": \"World\"
          }"
        ```
      "%
      = fun format x => %serialize% format (%force% x),

    deserialize
      : [| `Json, `Toml, `Yaml |] -> String -> Dyn
      | doc m%"
        Deserializes a string into a nickel value from the given representation.

        # Examples
        
        ```nickel
        deserialize `Json "{ \"hello\": \"Hello\", \"world\": \"World\" }"
          { hello = "Hello", world = "World" }
        ```
      "%
      = fun format x => %deserialize% format x,

    to_string
      | string.Stringable -> String
      | doc m%"
        Converts a stringable value to a string representation. Same as
        `string.from`.

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

    trace
      : forall a. String -> a -> a
      | doc m%"
        `builtin.trace msg x` prints `msg` to standard error, then proceeds with
        the evaluation of `x`.

        # Examples
        
        ```nickel
        builtin.trace "Hello, world!" true =>
          builtin.trace: Hello, world!
          true
        ```
      "%
      = fun msg x => %trace% msg x,

    FailWith
      | doc m%"
        A contract that always fails with the given message.

        # Examples
        
        ```nickel
        1 | FailWith "message" =>
          error: contract broken by a value: message
        ```
      "%
      = fun msg label value => contract.blame_with_message msg label,

    fail_with
      | String -> Dyn
      | doc m%"
        Abort evaluation with the given message.

        # Examples
        
        ```nickel
        fail_with "message" =>
          error: contract broken by a value: message
        ```
      "%
      = fun msg => null | FailWith msg,

  },
}
