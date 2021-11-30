{
  builtins = {
    isNum : Dyn -> Bool
    | doc m#"
      Checks if the given value is a number.

      For example:
      ```nickel
        isNum 1 =>
          true
        isNum "Hello, World!" =>
          false
      ```
      "#m
    = fun x => %isNum% x,

    isBool : Dyn -> Bool
    | doc m#"
      Checks if the given value is a boolean.

      For example:
      ```nickel
        isBool false =>
          true
        isBool 42 =>
          false
      ```
      "#m
    = fun x => %isBool% x,

    isStr : Dyn -> Bool
    | doc m#"
      Checks if the given value is a string.

      For example:
      ```nickel
        isBool true =>
          false
        isBool "Hello, World!" =>
          true
      ```
      "#m
    = fun x => %isStr% x,

    isFun : Dyn -> Bool
    | doc m#"
      Checks if the given value is a function.

      For example
      ```nickel
        isFun (fun x => x) =>
          true
        isFun 42 =>
          false
      ```
      "#m
    = fun x => %isFun% x,

    isList : Dyn -> Bool
    | doc m#"
      Checks if the given value is a list.

      For example
      ```nickel
        isList [ 1, 2 ] =>
          true
        isList 42 =>
          false
      ```
      "#m
    = fun x => %isList% x,

    isRecord : Dyn -> Bool
    | doc m#"
      Checks if the given value is a record.

      For example
      ```nickel
        isRecord [ 1, 2 ] =>
          false
        isRecord { hello = "Hello", world = "World" } =>
          true
      ```
      "#m
    = fun x => %isRecord% x,

    typeOf : Dyn -> <
      TypeNum,
      TypeBool,
      TypeStr,
      TypeFun,
      TypeList,
      TypeRecord,
      Other>
    | doc m#"
      Results in a value representing the type of the typed value.

      For example:
      ```nickel
        typeOf [ 1, 2 ] =>
          `TypeList
        typeOf (fun x => x) =>
          `TypeFun
      ```
      "#m
    = fun x =>
      if %isNum% x then `TypeNum
      else if %isBool% x then `TypeBool
      else if %isStr% x then `TypeStr
      else if %isFun% x then `TypeFun
      else if %isList% x then `TypeList
      else if %isRecord% x then `TypeRecord
      else `Other,

    seq : forall a. Dyn -> a -> a
    | doc m#"
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
      "#m
    = fun x y => %seq% x y,

    deepSeq : forall a. Dyn -> a -> a
    | doc m#"
      `deepSeq x y` forces a deep evaluation `x`, before resulting in `y`.

      For example:
      ```nickel
        deepSeq (42 / 0) 37 =>
          error
        deepSeq (42 / 2) 37 =>
          37
        deepSeq { tooFar = 42 / 0 } 37 =>
          error
      ```
      "#m
    = fun x y => %deepSeq% x y,

    hash : <Md5, Sha1, Sha256, Sha512> -> Str -> Str
    | doc m#"
      Hashes the given string provided the desired hash algorithm.

      For example:
      ```nickel
        hash `Md5 "hunter2" =>
          "2ab96390c7dbe3439de74d0c9b0b1767"
      ```
      "#m
    = fun type s => %hash% type s,

    serialize : <Json, Toml, Yaml> -> Dyn -> Str
    | doc m#"
      Serializes the given value to the desired representation.

      For example:
      ```nickel
        serialize `Json { hello = "Hello", world = "World" } =>
          "{
            "hello": "Hello",
            "world": "World"
          }"
      ```
      "#m
    = fun format x => %serialize% format (%deepSeq% x x),

    deserialize : <Json, Toml, Yaml> -> Str -> Dyn
    | doc m#"
      Deserializes the given string to a nickel value given the encoding of the string.

      For example:
      ```nickel
        deserialize `Json "{ \"hello\": \"Hello\", \"world\": \"World\" }"
          { hello = "Hello", world = "World" }
      ```
      "#m
    = fun format x => %deserialize% format x,
  }
}
