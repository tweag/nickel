{
  record = {
    map
      : forall a b. (String -> a -> b) -> { _ : a } -> { _ : b }
      | doc m%"
        Maps a function over every field of a record. The function is passed the
        name and value of each field.

        # Examples

        ```nickel
        map (fun s x => s) { hi = 2 } =>
          { hi = "hi" }
        map (fun s x => x + 1) { hello = 1, world = 2 } =>
          { hello = 2, world = 3 }
        ```
      "%
      = fun f r => %record_map% r f,

    fields
      : forall a. { _ : a } -> Array String
      | doc m%"
        Returns an array containing the names of all the fields of a record.

        # Examples

        ```nickel
        fields { one = 1, two = 2 } =>
          [ "one", "two" ]
        ```
      "%
      = fun r => %fields% r,

    values
      : forall a. { _ : a } -> Array a
      | doc m%"
        Returns an array containing the values of all the fields of a record.

        # Examples

        ```nickel
        values { one = 1, world = "world" }
          [ 1, "world" ]
        ```
      "%
      = fun r => %values% r,

    has_field
      : forall a. String -> { _ : a } -> Bool
      | doc m%"
        Given a string, checks if a record contains a field of that name.

        # Examples

        ```nickel
        has_field "hello" { one = 1, two = 2 } =>
          false
        has_field "one" { one = 1, two = 2 } =>
          true
        ```
      "%
      = fun field r => %has_field% field r,

    insert
      : forall a. String -> a -> { _ : a } -> { _ : a }
      | doc m%%"
          Inserts a new field into a record. `insert` doesn't mutate the original
          record but returns a new one instead.

          # Examples

          ```nickel
          insert "foo" 5 { bar = "bar" } =>
            { foo = 5, bar = "bar }
          {}
          |> insert "file.%{ext}" "data/text"
          |> insert "length" 10*1000 =>
            {"file.txt" = "data/text", "length" = 10000}
          ```
        "%%
      = fun field content r => %record_insert% field r content,

    remove
      : forall a. String -> { _ : a } -> { _ : a }
      | doc m%"
          Removes a field from a record. `remove` doesn't mutate the original
          record but returns a new one instead.

          # Examples

          ```nickel
          remove "foo" { foo = "foo", bar = "bar" } =>
            { bar = "bar }
          ```
        "%
      = fun field r => %record_remove% field r,

    update
      : forall a. String -> a -> { _ : a } -> { _ : a }
      | doc m%"
          Updates a field of a record with a new value. `update` doesn't mutate the
          original record but returns a new one instead. If the field to update is absent
          from the given record, `update` adds it.

          # Examples

          ```nickel
          update "foo" 5 { foo = "foo", bar = "bar" } =>
            { foo = 5, bar = "bar" }
          update "foo" 5 { bar = "bar" } =>
            { foo = 5, bar = "bar" }
          ```

          # Overriding

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
        let r =
          if %has_field% field r then
            %record_remove% field r
          else
            r
        in
        %record_insert% field r content,

    map_values
      : forall a b. (a -> b) -> { _ : a } -> { _ : b }
      | doc m%"
        Maps a function over the values of all the fields of a record.

        # Examples

        ```nickel
        map_values (fun x => x + 1) { hi = 2 } =>
          { hi = 3 }
        map (fun x => x + 1) { hello = 1, world = 2 } =>
          { hello = 2, world = 3 }
        ```
      "%
      = fun f => map (fun _field value => f value),

    to_array
      : forall a. { _ : a } -> Array { field : String, value : a }
      | doc m%"
        Converts a record to an array of key-value pairs.

        # Examples

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
        |> array.map (fun field' => { field = field', value = record."%{field'}" }),

    from_array
      : forall a. Array { field : String, value : a } -> { _ : a }
      | doc m%"
        Converts an array of key-value pairs into a record.

        # Examples

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

    is_empty
      : forall a. { _ : a } -> Bool
      | doc m%"
        Checks whether a record is empty.

        # Examples

        ```nickel
        is_empty {} => true
        is_empty { foo = 1 } => false
        ```
      "%
      = (==) {},

    merge_all
      | forall a. Array { _ : a } -> { _ : a }
      | doc m%"
        Merges an array of records.

        # Examples

        ```nickel
        merge_array [ { foo = 1 }, { bar = 2 } ]
          => { foo = 1, bar = 2 }
        ```
      "%
      = array.fold_left (&) {},

    filter
      : forall a. (String -> a -> Bool) -> { _ : a } -> { _ : a }
      | doc m%"
        `filter f r` returns a record containing all fields from `r` for which
        `f` returns true. The function `f` is passed the name and value of each field
        to make a decision.

        # Examples

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

    length
      : forall a. { _ : a } -> Number
      | doc m%"
          Returns the number of fields in a record. This count doesn't include
          fields both marked as optional and without a definition.

          Because of the need to filter empty optional fields, the cost of
          `length` is linear in the size of the record.
        "%
      = fun record =>
        record
        |> fields
        |> array.length,
  }
}
