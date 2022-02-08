{
  records = {
    map : forall a b. (Str -> a -> b) -> {_: a} -> {_: b}
    | doc m#"
      Maps a function on every field of a record. The string argument of the function argument is the name of the
      field.

      For example:
      ```nickel
        map (fun s x => s) { hi = 2 } =>
          { hi = "hi" }
        map (fun s x => x + 1) { hello = 1, world = 2 } =>
          { hello = 2, world = 3 }
      ```
      "#m
    = fun f r => %record_map% r f,

    fields | { ; Dyn} -> List Str
    | doc m#"
      Given a record, results in a list of the string representation of all fields in the record.

      ```nickel
        fields { one = 1, two = 2 } =>
          [ "one", "two" ]
      ```
      "#m
    = fun r => %fields% r,

    values | { ; Dyn} -> List
    | doc m#"
      Given a record, results in a list containing all the values in that record.

      ```nickel
        values { one = 1, world = "world" }
          [ 1, "world" ]
      ```
      "#m
    = fun r => %values% r,

    has_field : Str -> Dyn -> Bool
    | doc m#"
      Given the name of a field and a record, checks if the record contains the given field.

      ```nickel
        has_field "hello" { one = 1, two = 2 } =>
          false
        has_field "one" { one = 1, two = 2 } =>
          true
      "#m
    = fun field r => %has_field% field r,
  }
}
