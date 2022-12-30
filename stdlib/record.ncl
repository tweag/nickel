{
  record = {
    map : forall a b. (Str -> a -> b) -> {_: a} -> {_: b}
    | doc m%"
      Maps a function on every field of a record. The string argument of the function argument is the name of the
      field.

      For example:
      ```nickel
        map (fun s x => s) { hi = 2 } =>
          { hi = "hi" }
        map (fun s x => x + 1) { hello = 1, world = 2 } =>
          { hello = 2, world = 3 }
      ```
      "%
    = fun f r => %record_map% r f,

    fields : forall a. { _: a } -> Array Str
    | doc m%"
      Given a record, results in a array of the string representation of all fields in the record.

      ```nickel
        fields { one = 1, two = 2 } =>
          [ "one", "two" ]
      ```
      "%
    = fun r => %fields% r,

    values : forall a. { _: a } -> Array a
    | doc m%"
      Given a record, results in a array containing all the values in that record.

      ```nickel
        values { one = 1, world = "world" }
          [ 1, "world" ]
      ```
      "%
    = fun r => %values% r,

    has_field : forall a. Str -> {_ : a} -> Bool
    | doc m%"
      Given the name of a field and a record, checks if the record contains the given field.

      ```nickel
        has_field "hello" { one = 1, two = 2 } =>
          false
        has_field "one" { one = 1, two = 2 } =>
          true
      ```
      "%
    = fun field r => %has_field% field r,

    insert : forall a. Str -> a -> {_: a} -> {_: a}
      | doc m%%"
        Insert a new field in a record. `insert` doesn't mutate the original
        record but returns a new one instead.

        ```nickel
        insert "foo" foo { bar = "bar" } =>
          { foo = "foo", bar = "bar }

        {}
        |> insert "file.%{ext}" "data/text"
        |> insert "length" 10*1000 =>
          {"file.txt" = "data/text", "length" = 10000}
        ```
      "%%
      = fun field content r => %record_insert% field r content,

    remove : forall a. Str -> {_: a} -> {_: a}
      | doc m%"
        Remove a field from a record. `remove` doesn't mutate the original
        record but returns a new one instead.

        ```nickel
        remove "foo" foo { foo = "foo", bar = "bar" } =>
          { bar = "bar }
        ```
      "%
      = fun field r => %record_remove% field r,

    update | forall a. Str -> a -> {_: a} -> {_: a}
      | doc m%"
        Update a field of a record with a new value. `update` doesn't mutate the
        original record but returns a new one instead. If the field to update is absent
        from the given record, `update` simply adds it.

        ```nickel
        remove "foo" foo { foo = "foo", bar = "bar" } =>
          { bar = "bar" }
        ```

        As opposed to overriding a value with the merge operator `&`, `update`
        will only change the specified field and won't automatically update the other
        fields which depend on it:

        ```nickel
        { foo = bar + 1, bar | default = 0 } & { bar = 1 } =>
          { foo = 2, bar = 1 }
        update "bar" 1 {foo = bar + 1, bar | default = 0 } =>
          { foo = 1, bar = 1 }
        ```
      "%
      = fun field content r =>
        let r = if %has_field% field r then
          %record_remove% field r
        else
          r in
        %record_insert% field r content,
  }
}
