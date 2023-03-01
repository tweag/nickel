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

    update : forall a. Str -> a -> {_: a} -> {_: a}
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
    
    map_values : forall a b. (a -> b) -> {_: a} -> {_: b}
    | doc m%"
      Maps a function over every field value of a record.

      For example:
      ```nickel
        map_values (fun x => x + 1) { hi = 2 } =>
          { hi = 3 }
        map (fun x => x + 1) { hello = 1, world = 2 } =>
          { hello = 2, world = 3 }
      ```
    "%
    = fun f => map (fun _field value => f value),

    to_array : forall a. {_: a} -> Array {field: Str, value: a}
    | doc m%"
      Converts a record to an array of key-value pairs.

      For example:
      ```nickel
        to_array { hello = "world", foo = "bar" } =>
          [ { field = "hello", value = "world" },
            { field = "foo", value = "bar" }
          ]
      ```
    "%
    = fun record =>
      record
      |> fields
      |> array.map (fun field' => {field = field', value = record."%{field'}"}),

    from_array : forall a. Array { field: Str, value: a} -> {_: a}
    | doc m%"
      Convert an array of key-value pairs into a record.

      For example:
      ```nickel
        from_array [ { field = "hello", value = "world" },
                     { field = "foo", value = "bar" }
                   ]
          => { hello = "world", foo = "bar" }
      ```
    "%
    = fun bindings =>
      bindings
      |> array.map (fun binding => { "%{binding.field}" = binding.value })
      |> merge_all,

    is_empty : forall a. {_: a} -> Bool
    | doc m%"
      Check whether a record is empty.
      
      ```nickel
      is_empty {} => true
      is_empty { foo = 1 } => false
      ```
    "%
    = (==) {},

    merge_all | forall a. Array {_: a} -> {_: a}
    | doc m%"
      Merge an array of records.

      For example:
      ```nickel
      merge_array [ { foo = 1 }, { bar = 2 } ]
        => { foo = 1, bar = 2 }
      ```
    "%
    = array.fold_left (&) {},

    filter : forall a. (Str -> a -> Bool) -> {_: a} -> {_: a}
    | doc m%"
      Filter a record using the given function, which is passed the name and
      value for each key to make a decision.

      For example:
      ```nickel
      filter (fun name x => x % 2 == 0) { even = 2, odd = 3 }
        => { even = 2 }
      ```
    "%
    = fun f record =>
      record
      |> to_array
      |> array.filter (fun { field, value } => f field value)
      |> from_array,
  }
}
