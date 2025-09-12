# DO NOT EDIT
# This file was automatically generated using json-schema-to-nickel
let js2n = {
  Always
    | doc m%"
        A contract that always succeeds. The same as `Dyn`, but with a more JSON-schema name.
        
        # Examples
        
        ```nickel multiline
        "foo" | Always
        
        3 | Always
        ```
      "%
    = Dyn,
  Const
    | Dyn -> Dyn
    | doc m%"
        A more eager version of `std.contract.Equal`.
        
        # Examples
        
        ```nickel multiline
        5 | Const 5
        
        3 | Const 5
        # => error
        ```
      "%
    = fun expected =>
      std.contract.from_validator (fun value =>
        if value == expected then
          'Ok
        else
          'Error { message = "expected %{std.to_string expected}" }
      ),
  Enum
    | Array Dyn -> Dyn
    | doc m%"
        `Enum values x` succeeds if and only if `x` is equal to one of the elements of `values`.
        
        # Examples
        
        ```nickel multiline
        "no" | Enum [false, "false", "no", "off"]
        
        "yes" | Enum [false, "false", "no", "off"]
        # => error
        ```
      "%
    = fun variants =>
      std.contract.from_validator (fun value =>
        let check_equal = fun input variant =>
          (input == variant)
          || (
            (std.is_enum input)
            && (
              ((std.string.from_enum input) == variant)
                | Bool
            )
          )
        in
        if std.array.any (check_equal value) variants then
          'Ok
        else
          'Error {
            message = "expected one of %{std.serialize 'Json variants}"
          }
      ),
  IfThenElse
    | Dyn -> Dyn -> Dyn -> Dyn
    | doc m%"
        `IfThenElse iph zen els` checks the `iph` contract (eagerly). If that
        succeeds, it checks the `zen` contract and otherwise checks the `els`
        contract.
        
        Only the eager part of the `iph` contract is used to decide which of the
        other contracts is applied. The lazy part of the `iph` contract will be
        applied if the eager part succeeds.
        
        # Examples
        
        ```nickel multiline
        6 | IfThenElse Number (number.MultipleOf 2) String
        
        "hi" | IfThenElse Number (number.MultipleOf 2) String
        
        true | IfThenElse Number (number.MultipleOf 2) String
        # => error
        
        # The `zen` branch gets the value that was normalized by TagOrString
        "hi" | IfThenElse std.enum.TagOrString (std.contract.Equal 'hi) Number
        ```
      "%
    = fun iph zen els =>
      std.contract.custom (fun label value =>
        (match {
          'Ok v =>
            std.contract.check zen label v,
          'Error _ =>
            std.contract.check els label value,
        }
        ) (std.contract.check iph label value)
      ),
  Never
    | doc m%"
        A contract that always fails.
        
        # Examples
        
        ```nickel multiline
        "foo" | Never
        # => error
        
        3 | Never
        # => error
      "%
    = std.contract.from_predicate (fun _ => false),
  Null
    | doc m%"
        A contract asserting that a value is null.
        
        # Examples
        
        ```nickel multiline
        null | Null
        
        "foo" | Null
        # => error
      "%
    = std.contract.from_predicate ((fun x0 x1 => x0 == x1) null),
  Nullable
    : Dyn -> Dyn
    | doc m%"
        For a contract `C`, `Nullable C` returns a contract that allows either `C` or a null value.
        
        # Examples
        
        ```nickel multiline
        1 | Nullable Number
        
        null | Nullable Number
        
        "1" | Nullable Number
        # => error
        ```
      "%
    = fun Contract =>
      std.contract.custom (fun label value =>
        if value == null then
          'Ok value
        else
          std.contract.check Contract label value
      ),
  OneOf
    | Array Dyn -> Dyn
    | doc m%"
        Checks that exactly one of a list of contracts succeeds.
        
        # Examples
        
        ```nickel multiline
        "hi" | OneOf [Number, String, Bool]
        
        "hi" | OneOf [Number, String, std.enum.TagOrString]
        # => error: 2 of the contracts passed
        
        "hi" | OneOf [Number, std.enum.TagOrString]
        # => 'hi
        ```
      "%
    = fun contracts =>
      std.contract.custom (fun label value =>
        (match {
          'Count n =>
            'Error {
              message = "%{std.to_string
                            n} of the contracts passed, expected precisely one to pass"
            },
          'Ok v =>
            'Ok v,
        }
        ) (
          std.array.fold_right
            (fun ctr acc =>
              (match {
                'Ok val =>
                  (match {
                    'Count 0 =>
                      'Ok val,
                    'Count n =>
                      'Count (n + 1),
                    'Ok _ =>
                      'Count 2,
                  }
                  ) acc,
                'Error e =>
                  acc,
              }
              ) (std.contract.check ctr label value)
            )
            ('Count 0)
            contracts
        )
      ),
  array = {
    ArrayOf
      | Dyn -> Dyn
      | doc m%"
              An eager version of `Array`. `value | ArrayOf C` will succeed if `value` is an array and all of
              its elements satisfy the eager part of the contract `C`.
              
              # Examples
              
              ```nickel multiline
              [1, 2, 3] | ArrayOf Number
              # => [1, 2, 3]
              
              [1, 2, 3] | ArrayOf String
              # => error
              
              ["a", "b", "c"] | ArrayOf std.enum.TagOrString
              # => ['a, 'b, 'c]
              ```
            "%
      = fun ctr =>
        std.contract.custom (fun label value =>
          if !(std.is_array value) then
            'Error { message = "expected an array" }
          else
            std.array.try_fold_left
              (fun acc elt =>
                (match {
                  'Ok v =>
                    'Ok (acc @ [v]),
                  'Error e =>
                    'Error e,
                }
                ) (std.contract.check ctr label elt)
              )
              []
              value
        ),
    Contains
      | Dyn -> Dyn
      | doc m%"
              A contract checking that an array contains something satisfying another contract.
              
              For example, `Contains Number` is a contract checking that the value is an array
              and that it contains at least one number.
              
              This is an eager contract, and it assumes that the inputs are also eager.
              Any lazy parts of the inputs will be ignored.
              
              # Examples
              
              ```nickel multiline
              ["hi", true, 1] | Contains Bool
              # => ["hi", true, 1]
              
              ["hi", true, 1] | Contains (Array Bool)
              # => error
              
              # Lazy parts of the inner contract are ignored: "hi" does not get transformed
              # to 'hi.
              ["hi", true, 1] | Contains std.enum.TagOrString
              # => ["hi", true, 1]
              ```
            "%
      = fun contract =>
        std.contract.custom (fun label value =>
          if !(std.is_array value) then
            'Error { message = "expected an array" }
          else
            let good_elt = fun elt =>
              (match { 'Ok _ => true, 'Error _ => false, }) (std.contract.check contract label elt)
            in
            if std.array.any good_elt value then
              'Ok value
            else
              'Error { message = "no elements matched" }
        ),
    Items
      | Array Dyn -> Dyn -> Dyn
      | doc m%"
              A contract checking that the first few elements of an array satisfy
              specific contracts, and the remaining elements satisfy another contract.
              For example, `Items [C1 C2] C3` is a contract checking that the value is
              an array, that its first two elements (if present) satisfy `C1` and `C2`
              respectively, and that any other elements satisfy `C3`.
              
              # Examples
              
              ```nickel multiline
              [1, 2, "foo", "bar"] | Items [Number, Number] String
              
              # The value is allowed to be shorter than the list of contracts.
              [1] | Items [Number, Number] String
              
              [1, "foo", "bar"] | Items [Number, Number] String
              # => error
              
              # Inner contracts are propagated, but note that the overall `Items` contract
              # is eager if and only if all the contracts you pass in are eager.
              [1, { foo = "str"}, "foo", "bar"] | Items [Number, { foo | Number }] String
              # => error
              ```
            "%
      = fun elt_contracts rest =>
        std.contract.custom (fun label value =>
          if !(std.is_array value) then
            'Error { message = "expected an array" }
          else
            let length_to_check =
              std.number.min
                (std.array.length elt_contracts)
                (std.array.length value)
            in
            let { left, right, } = std.array.split_at length_to_check value
            in
            (match {
              'Error e =>
                'Error e,
              'Ok left_with_contracts =>
                std.array.try_fold_left
                  (fun acc elt =>
                    (match {
                      'Ok v =>
                        'Ok (acc @ [v]),
                      'Error e =>
                        'Error e,
                    }
                    ) (std.contract.check rest label elt)
                  )
                  left_with_contracts
                  right,
            }
            ) (
              std.array.try_fold_left
                (fun acc =>
                  match {
                    'Ok v =>
                      'Ok (acc @ [v]),
                    'Error e =>
                      'Error e,
                  }
                )
                []
                (
                  std.array.zip_with
                    (fun contract elt =>
                      std.contract.check contract label elt
                    )
                    elt_contracts
                    left
                )
            )
        ),
    MaxItems
      : Number -> Dyn
      | doc m%"
              A contract that checks whether an array has at most a certain number of items.
              
              # Examples
              
              ```nickel multiline
              [1, 2, 3] | MaxItems 3
              
              [1, 2, 3] | MaxItems 2
              # => error
            "%
      = fun n =>
        std.contract.from_predicate (fun arr =>
          (std.is_array arr)
          && ((std.array.length (arr | Array Dyn)) <= n)
        ),
    MinItems
      : Number -> Dyn
      | doc m%"
              A contract that checks whether an array has at least a certain number of items.
              
              # Examples
              
              ```nickel multiline
              [1, 2, 3] | MinItems 3
              
              [1, 2, 3] | MinItems 4
              # => error
              ```
            "%
      = fun n =>
        std.contract.from_predicate (fun arr =>
          (std.is_array arr)
          && ((std.array.length (arr | Array Dyn)) >= n)
        ),
    UniqueItems
      | doc m%"
              Checks that the elements of an array are pairwise distinct.
              
              # Examples
              
              ```nickel multiline
              [1, 2, "3"] | UniqueItems
              
              [1, 3, 2, 3] | UniqueItems
              # => error: duplicate found: 3
              ```
            "%
      =
        let find_duplicate
          : Array Dyn -> {
            has_duplicate : Bool,
            duplicate : Dyn,
            seen : { _ : Bool }
          }
          =
            std.array.fold_right
              (fun elt acc =>
                if acc.has_duplicate then
                  acc
                else
                  let index = std.serialize 'Json elt in
                  if std.record.has_field index acc.seen then
                    {
                      duplicate = elt,
                      has_duplicate = true,
                      seen = acc.seen
                    }
                  else
                    {
                      duplicate = null,
                      has_duplicate = false,
                      seen = std.record.insert index true acc.seen
                    }
              )
              { duplicate = null, has_duplicate = false, seen = {} }
        in
        std.contract.from_validator (fun x =>
          if !(std.is_array x) then
            'Error { message = "expected an array" }
          else
            let { has_duplicate, duplicate, .. } = find_duplicate x in
            if has_duplicate then
              'Error {
                message = "duplicate found: %{std.serialize
                                    'Json
                                    duplicate}"
              }
            else
              'Ok
        )
  },
  number =
    let from_predicate
      | (Number -> Bool) -> String -> Dyn
      = fun pred msg =>
        std.contract.from_validator (fun value =>
          if !(std.is_number value) then
            'Error { message = "expected a number" }
          else if pred (value | Number) then
            'Ok
          else
            'Error { message = msg }
        )
    in
    {
      ExclusiveMaximum
        | Number -> Dyn
        | doc m%"
                `ExclusiveMaximum n` is a contract checking that a number is at strictly less than `n`.
                
                # Examples
                
                ```nickel multiline
                5 | ExclusiveMaximum 5.1
                
                5 | ExclusiveMaximum 5
                # => error: expected an exclusive maximum of 5
                
                "5" | ExclusiveMaximum 5
                # => error: expected a number
                ```
              "%
        = fun limit =>
          from_predicate
            (fun x => x < limit)
            "expected an exclusive maximum of %{std.string.from_number
                      limit}",
      ExclusiveMinimum
        | Number -> Dyn
        | doc m%"
                `ExclusiveMinimum n` is a contract checking that a number is at strictly more than `n`.
                
                # Examples
                
                ```nickel multiline
                5 | ExclusiveMinimum 4.9
                
                5 | ExclusiveMinimum 5
                # => error: expected an exclusive minimum of 5
                
                "5" | ExclusiveMinimum 5
                # => error: expected a number
                ```
              "%
        = fun limit =>
          from_predicate
            (fun x => x > limit)
            "expected an exclusive minimum of %{std.string.from_number
                      limit}",
      Maximum
        | Number -> Dyn
        | doc m%"
                `Maximum n` is a contract checking that a number is at most `n`.
                
                # Examples
                
                ```nickel multiline
                5 | Maximum 5
                
                5 | Maximum 4
                # => error: expected a maximum of 4
                
                "5" | Maximum 5
                # => error: expected a number
                ```
              "%
        = fun limit =>
          from_predicate
            (fun x => x <= limit)
            "expected a maximum of %{std.string.from_number limit}",
      Minimum
        | Number -> Dyn
        | doc m%"
                `Minimum n` is a contract checking that a number is at least `n`.
                
                # Examples
                
                ```nickel multiline
                5 | Minimum 5
                
                5 | Minimum 6
                # => error: expected a minimum of 6
                
                "5" | Minimum 6
                # => error: expected a number
                ```
              "%
        = fun limit =>
          from_predicate
            (fun x => x >= limit)
            "expected a minimum of %{std.string.from_number limit}",
      MultipleOf
        | Number -> Dyn
        | doc m%"
                `MultipleOf n` is a contract checking whether a value is a multiple of `n`.
                
                # Examples
                
                ```nickel multiline
                4 | MultipleOf 2
                
                "4" | MultipleOf 2
                # => error: expected a number
                
                4 | MultipleOf 3
                # => error: expected a multiple of 3
                
                4 | MultipleOf 0
                # => error: expected a multiple of 0
                ```
              "%
        = fun n =>
          std.contract.from_validator (fun value =>
            if !(std.is_number value) then
              'Error { message = "expected a number" }
            else if n == 0 then
              'Error { message = "expected a multiple of 0" }
            else if std.number.is_integer (value / n) then
              'Ok
            else
              'Error {
                message = "expected a multiple of %{std.string.from_number
                                      n}"
              }
          )
    },
  record =
    let ErrData = {
      blame_location | Dyn | optional,
      message | String | optional,
      notes | Array String | optional
    }
    in
    {
      DependentContracts
        | { _ | Dyn } -> Dyn
        | doc m%"
                A contract for asserting that if some fields are present then the entire
                record satisfies a contract.
                
                Specifically, `DependentContracts { foo = C }` is a contract asserting that
                the value is a record, and if the value contains a field named `foo` then
                the entire record satisfies the contract `C`.
                
                `DependentContracts` calls `std.contract.check` for each of the provided
                contracts. In particular, if `C` is fully eager then so is
                `DependentContracts { foo = C }`.
                
                # Examples
                
                ```nickel multiline
                { foo = 1, bar = 2 } | DependentContracts { foo = { bar | Number, .. } }
                
                { foo = 1, bar = 2 } | DependentContracts { foo = { bar | Number } }
                # => error: extra field `foo`
                
                # This one doesn't fail: the inner contract never gets triggered because there's no `baz` field
                { foo = 1, bar = 2 } | DependentContracts { baz = { bar | Number } }
                
                1 | DependentContracts { baz = { bar | Number } }
                # => error: expected a record
                ```
              "%
        = fun deps =>
          std.contract.custom (fun label value =>
            if !(std.is_record value) then
              'Error { message = "expected a record" }
            else
              std.array.try_fold_left
                (fun acc { field, value = contract, } =>
                  if std.record.has_field field value then
                    std.contract.check contract label acc
                  else
                    'Ok acc
                )
                value
                (std.record.to_array deps)
          ),
      DependentFields
        | { _ | Array String } -> Dyn
        | doc m%"
                A contract for asserting that if some fields are present, other fields are
                also present.
                
                Specifically, `DependentFields { foo = ["bar", "baz"] }` is a contract
                asserting that if the field `foo` is present then the fields `bar` and
                `baz` are also present. This corresponds to the part of JSON schema's
                [dependencies](https://json-schema.org/draft-07/draft-handrews-json-schema-validation-01#rfc.section.6.5.7)
                keyword in which the values are arrays.
                
                This is an eager contract.
                
                ```nickel multiline
                { foo = 1, bar = 2 } | DependentFields { foo = ["bar"] }
                
                { foo = 1, baz = 2 } | DependentFields { foo = ["bar"] }
                # => error: expected field bar
                
                { baz = 2 } | DependentFields { foo = ["bar"] }
                ```
              "%
        = fun dependencies =>
          std.contract.from_validator (fun value =>
            if !(std.is_record value) then
              'Error { message = "expected a record" }
            else
              let must_have_fields
                : Array String -> { _ : Dyn } -> [| 'Ok, 'Error ErrData |]
                = fun fields x =>
                  std.array.fold_right
                    (fun field acc =>
                      if !(std.record.has_field field x) then
                        'Error (
                          { message = "expected field %{field}" }
                            | ErrData
                        )
                      else
                        acc
                    )
                    'Ok
                    fields
              in
              std.array.fold_right
                (fun { field, value = dependent_fields, } acc =>
                  if std.record.has_field field value then
                    (match {
                      'Ok =>
                        acc,
                      'Error e =>
                        'Error (
                          e
                          & {
                            notes = [
                              "because field %{field} was present"
                            ]
                          }
                        ),
                    }
                    ) (must_have_fields dependent_fields value)
                  else
                    acc
                )
                'Ok
                (std.record.to_array dependencies)
          ),
      MaxProperties
        | Number -> Dyn
        | doc m%"
                A contract that checks whether a record has at most a certain number of properties.
                
                # Examples
                
                ```nickel multiline
                { foo = 1, bar = 2 } | MaxProperties 2
                
                { foo = 1, bar = 2 } | MaxProperties 1
                # => error: expected a record with at most 1 fields
                ```
              "%
        = fun n =>
          std.contract.from_validator (fun value =>
            if !(std.is_record value) then
              'Error { message = "expected a record" }
            else if (std.record.length value) > n then
              'Error {
                message = "expected a record with at most %{std.string.from_number
                                    n} fields"
              }
            else
              'Ok
          ),
      MinProperties
        | Number -> Dyn
        | doc m%"
                A contract that checks whether a record has at least a certain number of properties.
                
                # Examples
                
                ```nickel multiline
                { foo = 1, bar = 2 } | MinProperties 2
                
                { foo = 1, bar = 2 } | MinProperties 3
                # => error: expected a record with at least 3 fields
                ```
              "%
        = fun n =>
          std.contract.from_validator (fun value =>
            if !(std.is_record value) then
              'Error { message = "expected a record" }
            else if (std.record.length value) < n then
              'Error {
                message = "expected a record with at least %{std.string.from_number
                                    n} fields"
              }
            else
              'Ok
          ),
      PropertyNames
        | Dyn -> Dyn
        | doc m%"
                `PropertyNames C` is a contract asserting that a value is a record and all of
                its field names satisfy the contract `C`.
                
                `PropertyNames C` is an eager contract for any `C`. Any lazy parts of `C` will
                be ignored.
                
                ```nickel multiline
                { foo = 1 } | PropertyNames std.string.NonEmpty
                
                { "1" = 1 } | PropertyNames std.string.NumberLiteral
                
                { foo = 1 } | PropertyNames std.string.NumberLiteral
                # => error: invalid number literal
                ```
              "%
        = fun contract =>
          std.contract.custom (fun label value =>
            if !(std.is_record value) then
              'Error { message = "expected a record" }
            else
              std.array.try_fold_left
                (fun acc field_name =>
                  (match {
                    'Ok _ =>
                      'Ok value,
                    'Error e =>
                      'Error e,
                  }
                  ) (std.contract.check contract label field_name)
                )
                ('Ok value)
                (std.record.fields value)
          ),
      Record
        | {
          additional | [| 'None, 'Some Dyn |],
          patterns | { _ | Dyn },
          properties | { _ | Dyn },
          required | { _ | Dyn }
        } -> Dyn
        | doc m%"
                A contract for checking JSON Schema object properties.
                
                In `Record { properties, required, patterns, additional }`, properties and patterns
                should be dicts containing contracts as values. These contracts will be
                applied to the appropriate fields of your record. `additional` can be
                `None` (in which case fields not matching `properties` or `patterns` are
                forbidden) or `Some contract` (in which case `contract` will be applied to
                every field not matched by `properties` and `patterns`).
                
                The contract returned by `Record` is sort of eager: it will call
                `std.contract.check` on the appropriate field values, and then it will
                propagate any lazy contracts returned by `std.contract.check`. This makes
                `Record` a little more eager than standard record contracts, and if all
                the contracts you provide are eager then `Record` will be fully eager.
                
                Cf. [https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-validation-01#section-6.5]
                
                # Examples
                
                ```nickel multiline
                { foo = 1 } | Record { properties = { foo = Number }, required = {}, patterns = {}, additional = 'None }
                # => { foo = 1 }
                
                # Properties aren't required to be present.
                { } | Record { properties = { foo = Number }, required = {}, patterns = {}, additional = 'None }
                # => { }
                
                # Properties aren't required to be present.
                { } | Record { properties = { foo = Number }, required = { foo = null }, patterns = {}, additional = 'None }
                # => error
                
                # If something in properties also matches a pattern, both contracts are applied.
                { foo = 1.1 } | Record { properties = { foo = Number }, required = {}, patterns = { "." = std.number.Integer }, additional = 'None }
                # => error: expected an integer
                
                # additional being 'None means that unmatched fields are forbidden
                { bar = 1 } | Record { properties = { foo = Number }, required = {}, patterns = {}, additional = 'None }
                # => error: extra fields
                
                # fields that are matched by a pattern don't count as "additional"
                { bar = 1 } | Record { properties = { foo = Number }, required = {}, patterns = { "." = Number }, additional = 'None }
                # => { bar = 1 }
                
                # additional being 'Some applies the extra contract to unmatched fields
                { bar = 1 } | Record { properties = { foo = Number }, required = {}, patterns = {}, additional = 'Some String }
                # => error
                
                # Inner contracts are propagated
                { foo = { bar = 1 }} | Record { properties = { foo = { bar | String } }, required = {}, patterns = {}, additional = 'None }
                # => error: contract broken by the value of `bar`
                
                # Inner contracts aren't evaluated eagerly
                ({ foo = { bar = 1 }, baz = 2} | Record { properties = { foo = { bar | String } }, required = {}, patterns = {}, additional = 'Some Dyn }).baz
                # => 2
                ```
              "%
        = fun { properties, required, patterns, additional, } =>
          std.contract.custom (fun label x =>
            if !(std.is_record x) then
              'Error { message = "expected a record" }
            else
              let x | { _ | Dyn } = x in
              let required | { _ | Dyn } = required in
              let checked_properties
                : [| 'Ok ({ _ | Dyn }), 'Error ErrData |]
                =
                  std.array.try_fold_left
                    (fun acc { field, value = contract, } =>
                      if !(std.record.has_field field x) then
                        if std.record.has_field field required then
                          'Error (
                            {
                              message = "missing required field `%{field}`"
                            }
                              | ErrData
                          )
                        else
                          'Ok acc
                      else
                        (match {
                          'Ok checked_value =>
                            'Ok (
                              std.record.insert
                                field
                                checked_value
                                acc
                            ),
                          'Error e =>
                            'Error e,
                        }
                        ) (
                          std.contract.check
                            contract
                            label
                            x."%{field}"
                        )
                    )
                    (
                      {}
                        | { _ | Dyn }
                    )
                    (std.record.to_array (properties | { _ : Dyn }))
              in
              let matching_fields
                : String -> { _ : Dyn }
                = fun pattern =>
                  let matcher = std.string.is_match pattern in
                  std.array.fold_left
                    (fun acc { field, value, } =>
                      if matcher field then
                        std.record.insert field value acc
                      else
                        acc
                    )
                    {}
                    (std.record.to_array x)
              in
              let checked_pattern_properties
                : [| 'Ok ({ _ | Dyn }), 'Error ErrData |]
                =
                  std.array.try_fold_left
                    (fun acc { field = pattern, value = contract, } =>
                      (match {
                        'Ok checked_dict =>
                          'Ok (
                            (acc & checked_dict)
                              | { _ | Dyn }
                          ),
                        'Error e =>
                          'Error e,
                      }
                      ) (
                        std.array.try_fold_left
                          (fun acc { field, value, } =>
                            (match {
                              'Ok checked_value =>
                                'Ok (
                                  std.record.insert
                                    field
                                    checked_value
                                    acc
                                ),
                              'Error e =>
                                'Error e,
                            }
                            ) (
                              std.contract.check
                                contract
                                label
                                value
                            )
                          )
                          (
                            {}
                              | { _ | Dyn }
                          )
                          (
                            std.record.to_array (matching_fields pattern)
                          )
                      )
                    )
                    (
                      {}
                        | { _ | Dyn }
                    )
                    (
                      std.record.to_array (
                        patterns
                          | { _ : Dyn }
                      )
                    )
              in
              (match {
                ['Error e, _] =>
                  'Error e,
                [_, 'Error e] =>
                  'Error e,
                [
                  'Ok checked_properties,
                  'Ok checked_pattern_properties
                ] =>
                  let remaining_fields
                    : { _ | Dyn }
                    =
                      std.array.fold_left
                        (fun acc field =>
                          if !(
                            std.record.has_field
                              field
                              acc
                          ) then
                            acc
                          else
                            std.record.remove field acc
                        )
                        x
                        (
                          (
                            std.record.fields (
                              checked_properties
                                | { _ : Dyn }
                            )
                          )
                          @ (
                            std.record.fields (
                              checked_pattern_properties
                                | { _ : Dyn }
                            )
                          )
                        )
                  in
                  let checked_additional
                    : [|
                      'Ok (
                        { _ | Dyn }
                      ),
                      'Error ErrData
                    |]
                    =
                      (match {
                        'None =>
                          if !(
                            std.record.is_empty remaining_fields
                          ) then
                            'Error (
                              {
                                message = "extra fields %{std.serialize
                                                          'Json
                                                          ((std.record.fields
                                                            remaining_fields)
                                                          | Dyn)}"
                              }
                                | ErrData
                            )
                          else
                            'Ok ({} | { _ | Dyn }),
                        'Some contract =>
                          std.array.try_fold_left
                            (fun acc { field, value, } =>
                              (match {
                                'Ok checked_value =>
                                  'Ok (
                                    std.record.insert
                                      field
                                      checked_value
                                      acc
                                  ),
                                'Error e =>
                                  'Error e,
                              }
                              ) (
                                std.contract.check
                                  contract
                                  label
                                  value
                              )
                            )
                            (
                              {}
                                | { _ | Dyn }
                            )
                            (
                              std.record.to_array remaining_fields
                            ),
                      }
                      ) (
                        additional
                          | [| 'None, 'Some Dyn |]
                      )
                  in
                  (match {
                    'Error e =>
                      'Error e,
                    'Ok checked_additional =>
                      'Ok (
                        (
                          checked_properties
                          & checked_pattern_properties
                        )
                        & checked_additional
                      ),
                  }
                  ) checked_additional,
              }
              ) [
                checked_properties,
                checked_pattern_properties
              ]
          ),
      Required
        | Array String -> Dyn
        | doc m%"
                A contract asserting that a record has certain fields.
                
                # Examples
                
                
                ```nickel multiline
                { foo = 1, bar = 2 } | Required [ "foo" ]
                # => { foo = 1, bar = 2 }
                
                { foo = 1, bar = 2 } | Required [ "baz" ]
                # => error: missing required field baz
                
                "baz" | Required [ "baz" ]
                # => error: expected a record
                ```
              "%
        = fun fields =>
          std.contract.from_validator (fun value =>
            if !(std.is_record value) then
              'Error { message = "expected a record" }
            else
              (match { 'Error e => 'Error e, 'Ok _ => 'Ok, }) (
                std.array.try_fold_left
                  (fun acc field =>
                    if !(std.record.has_field field value) then
                      'Error {
                        message = "missing required field %{field}"
                      }
                    else
                      'Ok acc
                  )
                  null
                  fields
              )
          )
    },
  string = {
    MaxLength
      | Number -> Dyn
      | doc m%"
              `MaxLength n` is a contract that checks for a string of length at most `n`.
              
              Here, the "length" of a string is the number of extended grapheme clusters in the string.
              This may not agree with the JSON schema semantics, which counts the number of Unicode
              code points.
              
              # Examples
              
              ```nickel multiline
              "Hello" | MaxLength 5
              
              # 15 bytes, 3 code points, 1 extended grapheme cluster
              "👨‍🦰" | MaxLength 1
              
              "Hello" | MaxLength 4
              # => error
              ```
            "%
      = fun limit =>
        std.contract.from_validator (fun value =>
          if !(std.is_string value) then
            'Error { message = "expected a string" }
          else if (std.string.length value) <= limit then
            'Ok
          else
            'Error {
              message = "expected a string of length at most %{std.string.from_number
                                  limit}"
            }
        ),
    MinLength
      | Number -> Dyn
      | doc m%"
              `MinLength n` is a contract that checks for a string of length at least `n`.
              
              Here, the "length" of a string is the number of extended grapheme clusters in the string.
              This may not agree with the JSON schema semantics, which counts the number of Unicode
              code points.
              
              # Examples
              
              ```nickel multiline
              "Hello" | MinLength 5
              
              # 15 bytes, 3 code points, 1 extended grapheme cluster
              "👨‍🦰" | MinLength 3
              # => error
              
              "Hello" | MinLength 6
              # => error
              ```
            "%
      = fun limit =>
        std.contract.from_validator (fun value =>
          if !(std.is_string value) then
            'Error { message = "expected a string" }
          else if (std.string.length value) >= limit then
            'Ok
          else
            'Error {
              message = "expected a string of length at least %{std.string.from_number
                                  limit}"
            }
        )
  }
}
in
let rec refs = {} in
{
  apiVersion | std.enum.TagOrString | [| 'v1 |] | optional,
  kind | std.enum.TagOrString | [| 'ReplicationController |] | optional,
  metadata
    | js2n.Nullable {
      annotations
        | js2n.Nullable ({ _ | js2n.Nullable String })
        | optional,
      creationTimestamp | js2n.Nullable String | optional,
      deletionGracePeriodSeconds
        | js2n.Nullable std.number.Integer
        | optional,
      deletionTimestamp | js2n.Nullable String | optional,
      finalizers
        | js2n.Nullable (Array (js2n.Nullable String))
        | optional,
      generateName | js2n.Nullable String | optional,
      generation | js2n.Nullable std.number.Integer | optional,
      labels | js2n.Nullable ({ _ | js2n.Nullable String }) | optional,
      managedFields
        | js2n.Nullable (
          Array (
            js2n.Nullable {
              apiVersion | js2n.Nullable String | optional,
              fieldsType | js2n.Nullable String | optional,
              fieldsV1 | js2n.Nullable ({ _ | Dyn }) | optional,
              manager | js2n.Nullable String | optional,
              operation | js2n.Nullable String | optional,
              subresource | js2n.Nullable String | optional,
              time | js2n.Nullable String | optional
            }
          )
        )
        | optional,
      name | js2n.Nullable String | optional,
      namespace | js2n.Nullable String | optional,
      ownerReferences
        | js2n.Nullable (
          Array (
            js2n.Nullable {
              apiVersion | String,
              blockOwnerDeletion | js2n.Nullable Bool | optional,
              controller | js2n.Nullable Bool | optional,
              kind | String,
              name | String,
              uid | String
            }
          )
        )
        | optional,
      resourceVersion | js2n.Nullable String | optional,
      selfLink | js2n.Nullable String | optional,
      uid | js2n.Nullable String | optional
    }
    | optional,
  spec
    | js2n.Nullable {
      minReadySeconds | js2n.Nullable std.number.Integer | optional,
      replicas | js2n.Nullable std.number.Integer | optional,
      selector | js2n.Nullable ({ _ | js2n.Nullable String }) | optional,
      template
        | js2n.Nullable {
          metadata
            | js2n.Nullable {
              annotations
                | js2n.Nullable ({ _ | js2n.Nullable String })
                | optional,
              creationTimestamp | js2n.Nullable String | optional,
              deletionGracePeriodSeconds
                | js2n.Nullable std.number.Integer
                | optional,
              deletionTimestamp | js2n.Nullable String | optional,
              finalizers
                | js2n.Nullable (Array (js2n.Nullable String))
                | optional,
              generateName | js2n.Nullable String | optional,
              generation
                | js2n.Nullable std.number.Integer
                | optional,
              labels
                | js2n.Nullable ({ _ | js2n.Nullable String })
                | optional,
              managedFields
                | js2n.Nullable (
                  Array (
                    js2n.Nullable {
                      apiVersion | js2n.Nullable String | optional,
                      fieldsType | js2n.Nullable String | optional,
                      fieldsV1
                        | js2n.Nullable ({ _ | Dyn })
                        | optional,
                      manager | js2n.Nullable String | optional,
                      operation | js2n.Nullable String | optional,
                      subresource | js2n.Nullable String | optional,
                      time | js2n.Nullable String | optional
                    }
                  )
                )
                | optional,
              name | js2n.Nullable String | optional,
              namespace | js2n.Nullable String | optional,
              ownerReferences
                | js2n.Nullable (
                  Array (
                    js2n.Nullable {
                      apiVersion | String,
                      blockOwnerDeletion
                        | js2n.Nullable Bool
                        | optional,
                      controller | js2n.Nullable Bool | optional,
                      kind | String,
                      name | String,
                      uid | String
                    }
                  )
                )
                | optional,
              resourceVersion | js2n.Nullable String | optional,
              selfLink | js2n.Nullable String | optional,
              uid | js2n.Nullable String | optional
            }
            | optional,
          spec
            | js2n.Nullable {
              activeDeadlineSeconds
                | js2n.Nullable std.number.Integer
                | optional,
              affinity
                | js2n.Nullable {
                  nodeAffinity
                    | js2n.Nullable {
                      preferredDuringSchedulingIgnoredDuringExecution
                        | js2n.Nullable (
                          Array (
                            js2n.Nullable {
                              preference
                                | {
                                  matchExpressions
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable {
                                          key | String,
                                          operator | String,
                                          values
                                            | js2n.Nullable (
                                              Array (
                                                js2n.Nullable String
                                              )
                                            )
                                            | optional
                                        }
                                      )
                                    )
                                    | optional,
                                  matchFields
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable {
                                          key | String,
                                          operator | String,
                                          values
                                            | js2n.Nullable (
                                              Array (
                                                js2n.Nullable String
                                              )
                                            )
                                            | optional
                                        }
                                      )
                                    )
                                    | optional
                                },
                              weight | std.number.Integer
                            }
                          )
                        )
                        | optional,
                      requiredDuringSchedulingIgnoredDuringExecution
                        | js2n.Nullable {
                          nodeSelectorTerms
                            | Array (
                              js2n.Nullable {
                                matchExpressions
                                  | js2n.Nullable (
                                    Array (
                                      js2n.Nullable {
                                        key | String,
                                        operator | String,
                                        values
                                          | js2n.Nullable (
                                            Array (
                                              js2n.Nullable String
                                            )
                                          )
                                          | optional
                                      }
                                    )
                                  )
                                  | optional,
                                matchFields
                                  | js2n.Nullable (
                                    Array (
                                      js2n.Nullable {
                                        key | String,
                                        operator | String,
                                        values
                                          | js2n.Nullable (
                                            Array (
                                              js2n.Nullable String
                                            )
                                          )
                                          | optional
                                      }
                                    )
                                  )
                                  | optional
                              }
                            )
                        }
                        | optional
                    }
                    | optional,
                  podAffinity
                    | js2n.Nullable {
                      preferredDuringSchedulingIgnoredDuringExecution
                        | js2n.Nullable (
                          Array (
                            js2n.Nullable {
                              podAffinityTerm
                                | {
                                  labelSelector
                                    | js2n.Nullable {
                                      matchExpressions
                                        | js2n.Nullable (
                                          Array (
                                            js2n.Nullable {
                                              key | String,
                                              operator
                                                | String,
                                              values
                                                | js2n.Nullable (
                                                  Array (
                                                    js2n.Nullable String
                                                  )
                                                )
                                                | optional
                                            }
                                          )
                                        )
                                        | optional,
                                      matchLabels
                                        | js2n.Nullable (
                                          {
                                            _ | js2n.Nullable String
                                          }
                                        )
                                        | optional
                                    }
                                    | optional,
                                  matchLabelKeys
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable String
                                      )
                                    )
                                    | optional,
                                  mismatchLabelKeys
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable String
                                      )
                                    )
                                    | optional,
                                  namespaceSelector
                                    | js2n.Nullable {
                                      matchExpressions
                                        | js2n.Nullable (
                                          Array (
                                            js2n.Nullable {
                                              key | String,
                                              operator
                                                | String,
                                              values
                                                | js2n.Nullable (
                                                  Array (
                                                    js2n.Nullable String
                                                  )
                                                )
                                                | optional
                                            }
                                          )
                                        )
                                        | optional,
                                      matchLabels
                                        | js2n.Nullable (
                                          {
                                            _ | js2n.Nullable String
                                          }
                                        )
                                        | optional
                                    }
                                    | optional,
                                  namespaces
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable String
                                      )
                                    )
                                    | optional,
                                  topologyKey | String
                                },
                              weight | std.number.Integer
                            }
                          )
                        )
                        | optional,
                      requiredDuringSchedulingIgnoredDuringExecution
                        | js2n.Nullable (
                          Array (
                            js2n.Nullable {
                              labelSelector
                                | js2n.Nullable {
                                  matchExpressions
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable {
                                          key | String,
                                          operator | String,
                                          values
                                            | js2n.Nullable (
                                              Array (
                                                js2n.Nullable String
                                              )
                                            )
                                            | optional
                                        }
                                      )
                                    )
                                    | optional,
                                  matchLabels
                                    | js2n.Nullable (
                                      {
                                        _ | js2n.Nullable String
                                      }
                                    )
                                    | optional
                                }
                                | optional,
                              matchLabelKeys
                                | js2n.Nullable (Array (js2n.Nullable String))
                                | optional,
                              mismatchLabelKeys
                                | js2n.Nullable (Array (js2n.Nullable String))
                                | optional,
                              namespaceSelector
                                | js2n.Nullable {
                                  matchExpressions
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable {
                                          key | String,
                                          operator | String,
                                          values
                                            | js2n.Nullable (
                                              Array (
                                                js2n.Nullable String
                                              )
                                            )
                                            | optional
                                        }
                                      )
                                    )
                                    | optional,
                                  matchLabels
                                    | js2n.Nullable (
                                      {
                                        _ | js2n.Nullable String
                                      }
                                    )
                                    | optional
                                }
                                | optional,
                              namespaces
                                | js2n.Nullable (Array (js2n.Nullable String))
                                | optional,
                              topologyKey | String
                            }
                          )
                        )
                        | optional
                    }
                    | optional,
                  podAntiAffinity
                    | js2n.Nullable {
                      preferredDuringSchedulingIgnoredDuringExecution
                        | js2n.Nullable (
                          Array (
                            js2n.Nullable {
                              podAffinityTerm
                                | {
                                  labelSelector
                                    | js2n.Nullable {
                                      matchExpressions
                                        | js2n.Nullable (
                                          Array (
                                            js2n.Nullable {
                                              key | String,
                                              operator
                                                | String,
                                              values
                                                | js2n.Nullable (
                                                  Array (
                                                    js2n.Nullable String
                                                  )
                                                )
                                                | optional
                                            }
                                          )
                                        )
                                        | optional,
                                      matchLabels
                                        | js2n.Nullable (
                                          {
                                            _ | js2n.Nullable String
                                          }
                                        )
                                        | optional
                                    }
                                    | optional,
                                  matchLabelKeys
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable String
                                      )
                                    )
                                    | optional,
                                  mismatchLabelKeys
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable String
                                      )
                                    )
                                    | optional,
                                  namespaceSelector
                                    | js2n.Nullable {
                                      matchExpressions
                                        | js2n.Nullable (
                                          Array (
                                            js2n.Nullable {
                                              key | String,
                                              operator
                                                | String,
                                              values
                                                | js2n.Nullable (
                                                  Array (
                                                    js2n.Nullable String
                                                  )
                                                )
                                                | optional
                                            }
                                          )
                                        )
                                        | optional,
                                      matchLabels
                                        | js2n.Nullable (
                                          {
                                            _ | js2n.Nullable String
                                          }
                                        )
                                        | optional
                                    }
                                    | optional,
                                  namespaces
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable String
                                      )
                                    )
                                    | optional,
                                  topologyKey | String
                                },
                              weight | std.number.Integer
                            }
                          )
                        )
                        | optional,
                      requiredDuringSchedulingIgnoredDuringExecution
                        | js2n.Nullable (
                          Array (
                            js2n.Nullable {
                              labelSelector
                                | js2n.Nullable {
                                  matchExpressions
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable {
                                          key | String,
                                          operator | String,
                                          values
                                            | js2n.Nullable (
                                              Array (
                                                js2n.Nullable String
                                              )
                                            )
                                            | optional
                                        }
                                      )
                                    )
                                    | optional,
                                  matchLabels
                                    | js2n.Nullable (
                                      {
                                        _ | js2n.Nullable String
                                      }
                                    )
                                    | optional
                                }
                                | optional,
                              matchLabelKeys
                                | js2n.Nullable (Array (js2n.Nullable String))
                                | optional,
                              mismatchLabelKeys
                                | js2n.Nullable (Array (js2n.Nullable String))
                                | optional,
                              namespaceSelector
                                | js2n.Nullable {
                                  matchExpressions
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable {
                                          key | String,
                                          operator | String,
                                          values
                                            | js2n.Nullable (
                                              Array (
                                                js2n.Nullable String
                                              )
                                            )
                                            | optional
                                        }
                                      )
                                    )
                                    | optional,
                                  matchLabels
                                    | js2n.Nullable (
                                      {
                                        _ | js2n.Nullable String
                                      }
                                    )
                                    | optional
                                }
                                | optional,
                              namespaces
                                | js2n.Nullable (Array (js2n.Nullable String))
                                | optional,
                              topologyKey | String
                            }
                          )
                        )
                        | optional
                    }
                    | optional
                }
                | optional,
              automountServiceAccountToken
                | js2n.Nullable Bool
                | optional,
              containers
                | Array (
                  js2n.Nullable {
                    args
                      | js2n.Nullable (Array (js2n.Nullable String))
                      | optional,
                    command
                      | js2n.Nullable (Array (js2n.Nullable String))
                      | optional,
                    env
                      | js2n.Nullable (
                        Array (
                          js2n.Nullable {
                            name | String,
                            value
                              | js2n.Nullable String
                              | optional,
                            valueFrom
                              | js2n.Nullable {
                                configMapKeyRef
                                  | js2n.Nullable {
                                    key | String,
                                    name
                                      | js2n.Nullable String
                                      | optional,
                                    "optional"
                                      | js2n.Nullable Bool
                                      | optional
                                  }
                                  | optional,
                                fieldRef
                                  | js2n.Nullable {
                                    apiVersion
                                      | js2n.Nullable String
                                      | optional,
                                    fieldPath | String
                                  }
                                  | optional,
                                fileKeyRef
                                  | js2n.Nullable {
                                    key | String,
                                    "optional"
                                      | js2n.Nullable Bool
                                      | optional,
                                    path | String,
                                    volumeName | String
                                  }
                                  | optional,
                                resourceFieldRef
                                  | js2n.Nullable {
                                    containerName
                                      | js2n.Nullable String
                                      | optional,
                                    divisor
                                      | js2n.OneOf [
                                        js2n.Nullable String,
                                        js2n.Nullable Number
                                      ]
                                      | optional,
                                    resource | String
                                  }
                                  | optional,
                                secretKeyRef
                                  | js2n.Nullable {
                                    key | String,
                                    name
                                      | js2n.Nullable String
                                      | optional,
                                    "optional"
                                      | js2n.Nullable Bool
                                      | optional
                                  }
                                  | optional
                              }
                              | optional
                          }
                        )
                      )
                      | optional,
                    envFrom
                      | js2n.Nullable (
                        Array (
                          js2n.Nullable {
                            configMapRef
                              | js2n.Nullable {
                                name
                                  | js2n.Nullable String
                                  | optional,
                                "optional"
                                  | js2n.Nullable Bool
                                  | optional
                              }
                              | optional,
                            prefix
                              | js2n.Nullable String
                              | optional,
                            secretRef
                              | js2n.Nullable {
                                name
                                  | js2n.Nullable String
                                  | optional,
                                "optional"
                                  | js2n.Nullable Bool
                                  | optional
                              }
                              | optional
                          }
                        )
                      )
                      | optional,
                    image | js2n.Nullable String | optional,
                    imagePullPolicy
                      | js2n.Nullable String
                      | optional,
                    lifecycle
                      | js2n.Nullable {
                        postStart
                          | js2n.Nullable {
                            exec
                              | js2n.Nullable {
                                command
                                  | js2n.Nullable (
                                    Array (
                                      js2n.Nullable String
                                    )
                                  )
                                  | optional
                              }
                              | optional,
                            httpGet
                              | js2n.Nullable {
                                host
                                  | js2n.Nullable String
                                  | optional,
                                httpHeaders
                                  | js2n.Nullable (
                                    Array (
                                      js2n.Nullable {
                                        name | String,
                                        value | String
                                      }
                                    )
                                  )
                                  | optional,
                                path
                                  | js2n.Nullable String
                                  | optional,
                                port
                                  | js2n.OneOf [
                                    js2n.Nullable String,
                                    js2n.Nullable std.number.Integer
                                  ],
                                scheme
                                  | js2n.Nullable String
                                  | optional
                              }
                              | optional,
                            sleep
                              | js2n.Nullable { seconds | std.number.Integer }
                              | optional,
                            tcpSocket
                              | js2n.Nullable {
                                host
                                  | js2n.Nullable String
                                  | optional,
                                port
                                  | js2n.OneOf [
                                    js2n.Nullable String,
                                    js2n.Nullable std.number.Integer
                                  ]
                              }
                              | optional
                          }
                          | optional,
                        preStop
                          | js2n.Nullable {
                            exec
                              | js2n.Nullable {
                                command
                                  | js2n.Nullable (
                                    Array (
                                      js2n.Nullable String
                                    )
                                  )
                                  | optional
                              }
                              | optional,
                            httpGet
                              | js2n.Nullable {
                                host
                                  | js2n.Nullable String
                                  | optional,
                                httpHeaders
                                  | js2n.Nullable (
                                    Array (
                                      js2n.Nullable {
                                        name | String,
                                        value | String
                                      }
                                    )
                                  )
                                  | optional,
                                path
                                  | js2n.Nullable String
                                  | optional,
                                port
                                  | js2n.OneOf [
                                    js2n.Nullable String,
                                    js2n.Nullable std.number.Integer
                                  ],
                                scheme
                                  | js2n.Nullable String
                                  | optional
                              }
                              | optional,
                            sleep
                              | js2n.Nullable { seconds | std.number.Integer }
                              | optional,
                            tcpSocket
                              | js2n.Nullable {
                                host
                                  | js2n.Nullable String
                                  | optional,
                                port
                                  | js2n.OneOf [
                                    js2n.Nullable String,
                                    js2n.Nullable std.number.Integer
                                  ]
                              }
                              | optional
                          }
                          | optional,
                        stopSignal
                          | js2n.Nullable String
                          | optional
                      }
                      | optional,
                    livenessProbe
                      | js2n.Nullable {
                        exec
                          | js2n.Nullable {
                            command
                              | js2n.Nullable (Array (js2n.Nullable String))
                              | optional
                          }
                          | optional,
                        failureThreshold
                          | js2n.Nullable std.number.Integer
                          | optional,
                        grpc
                          | js2n.Nullable {
                            port | std.number.Integer,
                            service
                              | js2n.Nullable String
                              | optional
                          }
                          | optional,
                        httpGet
                          | js2n.Nullable {
                            host
                              | js2n.Nullable String
                              | optional,
                            httpHeaders
                              | js2n.Nullable (
                                Array (
                                  js2n.Nullable {
                                    name | String,
                                    value | String
                                  }
                                )
                              )
                              | optional,
                            path
                              | js2n.Nullable String
                              | optional,
                            port
                              | js2n.OneOf [
                                js2n.Nullable String,
                                js2n.Nullable std.number.Integer
                              ],
                            scheme
                              | js2n.Nullable String
                              | optional
                          }
                          | optional,
                        initialDelaySeconds
                          | js2n.Nullable std.number.Integer
                          | optional,
                        periodSeconds
                          | js2n.Nullable std.number.Integer
                          | optional,
                        successThreshold
                          | js2n.Nullable std.number.Integer
                          | optional,
                        tcpSocket
                          | js2n.Nullable {
                            host
                              | js2n.Nullable String
                              | optional,
                            port
                              | js2n.OneOf [
                                js2n.Nullable String,
                                js2n.Nullable std.number.Integer
                              ]
                          }
                          | optional,
                        terminationGracePeriodSeconds
                          | js2n.Nullable std.number.Integer
                          | optional,
                        timeoutSeconds
                          | js2n.Nullable std.number.Integer
                          | optional
                      }
                      | optional,
                    name | String,
                    ports
                      | js2n.Nullable (
                        Array (
                          js2n.Nullable {
                            containerPort | std.number.Integer,
                            hostIP
                              | js2n.Nullable String
                              | optional,
                            hostPort
                              | js2n.Nullable std.number.Integer
                              | optional,
                            name
                              | js2n.Nullable String
                              | optional,
                            protocol
                              | js2n.Nullable String
                              | optional
                          }
                        )
                      )
                      | optional,
                    readinessProbe
                      | js2n.Nullable {
                        exec
                          | js2n.Nullable {
                            command
                              | js2n.Nullable (Array (js2n.Nullable String))
                              | optional
                          }
                          | optional,
                        failureThreshold
                          | js2n.Nullable std.number.Integer
                          | optional,
                        grpc
                          | js2n.Nullable {
                            port | std.number.Integer,
                            service
                              | js2n.Nullable String
                              | optional
                          }
                          | optional,
                        httpGet
                          | js2n.Nullable {
                            host
                              | js2n.Nullable String
                              | optional,
                            httpHeaders
                              | js2n.Nullable (
                                Array (
                                  js2n.Nullable {
                                    name | String,
                                    value | String
                                  }
                                )
                              )
                              | optional,
                            path
                              | js2n.Nullable String
                              | optional,
                            port
                              | js2n.OneOf [
                                js2n.Nullable String,
                                js2n.Nullable std.number.Integer
                              ],
                            scheme
                              | js2n.Nullable String
                              | optional
                          }
                          | optional,
                        initialDelaySeconds
                          | js2n.Nullable std.number.Integer
                          | optional,
                        periodSeconds
                          | js2n.Nullable std.number.Integer
                          | optional,
                        successThreshold
                          | js2n.Nullable std.number.Integer
                          | optional,
                        tcpSocket
                          | js2n.Nullable {
                            host
                              | js2n.Nullable String
                              | optional,
                            port
                              | js2n.OneOf [
                                js2n.Nullable String,
                                js2n.Nullable std.number.Integer
                              ]
                          }
                          | optional,
                        terminationGracePeriodSeconds
                          | js2n.Nullable std.number.Integer
                          | optional,
                        timeoutSeconds
                          | js2n.Nullable std.number.Integer
                          | optional
                      }
                      | optional,
                    resizePolicy
                      | js2n.Nullable (
                        Array (
                          js2n.Nullable {
                            resourceName | String,
                            restartPolicy | String
                          }
                        )
                      )
                      | optional,
                    resources
                      | js2n.Nullable {
                        claims
                          | js2n.Nullable (
                            Array (
                              js2n.Nullable {
                                name | String,
                                request
                                  | js2n.Nullable String
                                  | optional
                              }
                            )
                          )
                          | optional,
                        limits
                          | js2n.Nullable (
                            {
                              _ | js2n.OneOf [
                                js2n.Nullable String,
                                js2n.Nullable Number
                              ]
                            }
                          )
                          | optional,
                        requests
                          | js2n.Nullable (
                            {
                              _ | js2n.OneOf [
                                js2n.Nullable String,
                                js2n.Nullable Number
                              ]
                            }
                          )
                          | optional
                      }
                      | optional,
                    restartPolicy | js2n.Nullable String | optional,
                    restartPolicyRules
                      | js2n.Nullable (
                        Array (
                          js2n.Nullable {
                            action | String,
                            exitCodes
                              | js2n.Nullable {
                                operator | String,
                                values
                                  | js2n.Nullable (
                                    Array (
                                      js2n.Nullable std.number.Integer
                                    )
                                  )
                                  | optional
                              }
                              | optional
                          }
                        )
                      )
                      | optional,
                    securityContext
                      | js2n.Nullable {
                        allowPrivilegeEscalation
                          | js2n.Nullable Bool
                          | optional,
                        appArmorProfile
                          | js2n.Nullable {
                            localhostProfile
                              | js2n.Nullable String
                              | optional,
                            type | String
                          }
                          | optional,
                        capabilities
                          | js2n.Nullable {
                            add
                              | js2n.Nullable (Array (js2n.Nullable String))
                              | optional,
                            drop
                              | js2n.Nullable (Array (js2n.Nullable String))
                              | optional
                          }
                          | optional,
                        privileged
                          | js2n.Nullable Bool
                          | optional,
                        procMount
                          | js2n.Nullable String
                          | optional,
                        readOnlyRootFilesystem
                          | js2n.Nullable Bool
                          | optional,
                        runAsGroup
                          | js2n.Nullable std.number.Integer
                          | optional,
                        runAsNonRoot
                          | js2n.Nullable Bool
                          | optional,
                        runAsUser
                          | js2n.Nullable std.number.Integer
                          | optional,
                        seLinuxOptions
                          | js2n.Nullable {
                            level
                              | js2n.Nullable String
                              | optional,
                            role
                              | js2n.Nullable String
                              | optional,
                            type
                              | js2n.Nullable String
                              | optional,
                            user
                              | js2n.Nullable String
                              | optional
                          }
                          | optional,
                        seccompProfile
                          | js2n.Nullable {
                            localhostProfile
                              | js2n.Nullable String
                              | optional,
                            type | String
                          }
                          | optional,
                        windowsOptions
                          | js2n.Nullable {
                            gmsaCredentialSpec
                              | js2n.Nullable String
                              | optional,
                            gmsaCredentialSpecName
                              | js2n.Nullable String
                              | optional,
                            hostProcess
                              | js2n.Nullable Bool
                              | optional,
                            runAsUserName
                              | js2n.Nullable String
                              | optional
                          }
                          | optional
                      }
                      | optional,
                    startupProbe
                      | js2n.Nullable {
                        exec
                          | js2n.Nullable {
                            command
                              | js2n.Nullable (Array (js2n.Nullable String))
                              | optional
                          }
                          | optional,
                        failureThreshold
                          | js2n.Nullable std.number.Integer
                          | optional,
                        grpc
                          | js2n.Nullable {
                            port | std.number.Integer,
                            service
                              | js2n.Nullable String
                              | optional
                          }
                          | optional,
                        httpGet
                          | js2n.Nullable {
                            host
                              | js2n.Nullable String
                              | optional,
                            httpHeaders
                              | js2n.Nullable (
                                Array (
                                  js2n.Nullable {
                                    name | String,
                                    value | String
                                  }
                                )
                              )
                              | optional,
                            path
                              | js2n.Nullable String
                              | optional,
                            port
                              | js2n.OneOf [
                                js2n.Nullable String,
                                js2n.Nullable std.number.Integer
                              ],
                            scheme
                              | js2n.Nullable String
                              | optional
                          }
                          | optional,
                        initialDelaySeconds
                          | js2n.Nullable std.number.Integer
                          | optional,
                        periodSeconds
                          | js2n.Nullable std.number.Integer
                          | optional,
                        successThreshold
                          | js2n.Nullable std.number.Integer
                          | optional,
                        tcpSocket
                          | js2n.Nullable {
                            host
                              | js2n.Nullable String
                              | optional,
                            port
                              | js2n.OneOf [
                                js2n.Nullable String,
                                js2n.Nullable std.number.Integer
                              ]
                          }
                          | optional,
                        terminationGracePeriodSeconds
                          | js2n.Nullable std.number.Integer
                          | optional,
                        timeoutSeconds
                          | js2n.Nullable std.number.Integer
                          | optional
                      }
                      | optional,
                    stdin | js2n.Nullable Bool | optional,
                    stdinOnce | js2n.Nullable Bool | optional,
                    terminationMessagePath
                      | js2n.Nullable String
                      | optional,
                    terminationMessagePolicy
                      | js2n.Nullable String
                      | optional,
                    tty | js2n.Nullable Bool | optional,
                    volumeDevices
                      | js2n.Nullable (
                        Array (
                          js2n.Nullable { devicePath | String, name | String }
                        )
                      )
                      | optional,
                    volumeMounts
                      | js2n.Nullable (
                        Array (
                          js2n.Nullable {
                            mountPath | String,
                            mountPropagation
                              | js2n.Nullable String
                              | optional,
                            name | String,
                            readOnly
                              | js2n.Nullable Bool
                              | optional,
                            recursiveReadOnly
                              | js2n.Nullable String
                              | optional,
                            subPath
                              | js2n.Nullable String
                              | optional,
                            subPathExpr
                              | js2n.Nullable String
                              | optional
                          }
                        )
                      )
                      | optional,
                    workingDir | js2n.Nullable String | optional
                  }
                ),
              dnsConfig
                | js2n.Nullable {
                  nameservers
                    | js2n.Nullable (Array (js2n.Nullable String))
                    | optional,
                  options
                    | js2n.Nullable (
                      Array (
                        js2n.Nullable {
                          name | js2n.Nullable String | optional,
                          value | js2n.Nullable String | optional
                        }
                      )
                    )
                    | optional,
                  searches
                    | js2n.Nullable (Array (js2n.Nullable String))
                    | optional
                }
                | optional,
              dnsPolicy | js2n.Nullable String | optional,
              enableServiceLinks | js2n.Nullable Bool | optional,
              ephemeralContainers
                | js2n.Nullable (
                  Array (
                    js2n.Nullable {
                      args
                        | js2n.Nullable (Array (js2n.Nullable String))
                        | optional,
                      command
                        | js2n.Nullable (Array (js2n.Nullable String))
                        | optional,
                      env
                        | js2n.Nullable (
                          Array (
                            js2n.Nullable {
                              name | String,
                              value
                                | js2n.Nullable String
                                | optional,
                              valueFrom
                                | js2n.Nullable {
                                  configMapKeyRef
                                    | js2n.Nullable {
                                      key | String,
                                      name
                                        | js2n.Nullable String
                                        | optional,
                                      "optional"
                                        | js2n.Nullable Bool
                                        | optional
                                    }
                                    | optional,
                                  fieldRef
                                    | js2n.Nullable {
                                      apiVersion
                                        | js2n.Nullable String
                                        | optional,
                                      fieldPath | String
                                    }
                                    | optional,
                                  fileKeyRef
                                    | js2n.Nullable {
                                      key | String,
                                      "optional"
                                        | js2n.Nullable Bool
                                        | optional,
                                      path | String,
                                      volumeName | String
                                    }
                                    | optional,
                                  resourceFieldRef
                                    | js2n.Nullable {
                                      containerName
                                        | js2n.Nullable String
                                        | optional,
                                      divisor
                                        | js2n.OneOf [
                                          js2n.Nullable String,
                                          js2n.Nullable Number
                                        ]
                                        | optional,
                                      resource | String
                                    }
                                    | optional,
                                  secretKeyRef
                                    | js2n.Nullable {
                                      key | String,
                                      name
                                        | js2n.Nullable String
                                        | optional,
                                      "optional"
                                        | js2n.Nullable Bool
                                        | optional
                                    }
                                    | optional
                                }
                                | optional
                            }
                          )
                        )
                        | optional,
                      envFrom
                        | js2n.Nullable (
                          Array (
                            js2n.Nullable {
                              configMapRef
                                | js2n.Nullable {
                                  name
                                    | js2n.Nullable String
                                    | optional,
                                  "optional"
                                    | js2n.Nullable Bool
                                    | optional
                                }
                                | optional,
                              prefix
                                | js2n.Nullable String
                                | optional,
                              secretRef
                                | js2n.Nullable {
                                  name
                                    | js2n.Nullable String
                                    | optional,
                                  "optional"
                                    | js2n.Nullable Bool
                                    | optional
                                }
                                | optional
                            }
                          )
                        )
                        | optional,
                      image | js2n.Nullable String | optional,
                      imagePullPolicy
                        | js2n.Nullable String
                        | optional,
                      lifecycle
                        | js2n.Nullable {
                          postStart
                            | js2n.Nullable {
                              exec
                                | js2n.Nullable {
                                  command
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable String
                                      )
                                    )
                                    | optional
                                }
                                | optional,
                              httpGet
                                | js2n.Nullable {
                                  host
                                    | js2n.Nullable String
                                    | optional,
                                  httpHeaders
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable {
                                          name | String,
                                          value | String
                                        }
                                      )
                                    )
                                    | optional,
                                  path
                                    | js2n.Nullable String
                                    | optional,
                                  port
                                    | js2n.OneOf [
                                      js2n.Nullable String,
                                      js2n.Nullable std.number.Integer
                                    ],
                                  scheme
                                    | js2n.Nullable String
                                    | optional
                                }
                                | optional,
                              sleep
                                | js2n.Nullable {
                                  seconds | std.number.Integer
                                }
                                | optional,
                              tcpSocket
                                | js2n.Nullable {
                                  host
                                    | js2n.Nullable String
                                    | optional,
                                  port
                                    | js2n.OneOf [
                                      js2n.Nullable String,
                                      js2n.Nullable std.number.Integer
                                    ]
                                }
                                | optional
                            }
                            | optional,
                          preStop
                            | js2n.Nullable {
                              exec
                                | js2n.Nullable {
                                  command
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable String
                                      )
                                    )
                                    | optional
                                }
                                | optional,
                              httpGet
                                | js2n.Nullable {
                                  host
                                    | js2n.Nullable String
                                    | optional,
                                  httpHeaders
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable {
                                          name | String,
                                          value | String
                                        }
                                      )
                                    )
                                    | optional,
                                  path
                                    | js2n.Nullable String
                                    | optional,
                                  port
                                    | js2n.OneOf [
                                      js2n.Nullable String,
                                      js2n.Nullable std.number.Integer
                                    ],
                                  scheme
                                    | js2n.Nullable String
                                    | optional
                                }
                                | optional,
                              sleep
                                | js2n.Nullable {
                                  seconds | std.number.Integer
                                }
                                | optional,
                              tcpSocket
                                | js2n.Nullable {
                                  host
                                    | js2n.Nullable String
                                    | optional,
                                  port
                                    | js2n.OneOf [
                                      js2n.Nullable String,
                                      js2n.Nullable std.number.Integer
                                    ]
                                }
                                | optional
                            }
                            | optional,
                          stopSignal
                            | js2n.Nullable String
                            | optional
                        }
                        | optional,
                      livenessProbe
                        | js2n.Nullable {
                          exec
                            | js2n.Nullable {
                              command
                                | js2n.Nullable (Array (js2n.Nullable String))
                                | optional
                            }
                            | optional,
                          failureThreshold
                            | js2n.Nullable std.number.Integer
                            | optional,
                          grpc
                            | js2n.Nullable {
                              port | std.number.Integer,
                              service
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          httpGet
                            | js2n.Nullable {
                              host
                                | js2n.Nullable String
                                | optional,
                              httpHeaders
                                | js2n.Nullable (
                                  Array (
                                    js2n.Nullable {
                                      name | String,
                                      value | String
                                    }
                                  )
                                )
                                | optional,
                              path
                                | js2n.Nullable String
                                | optional,
                              port
                                | js2n.OneOf [
                                  js2n.Nullable String,
                                  js2n.Nullable std.number.Integer
                                ],
                              scheme
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          initialDelaySeconds
                            | js2n.Nullable std.number.Integer
                            | optional,
                          periodSeconds
                            | js2n.Nullable std.number.Integer
                            | optional,
                          successThreshold
                            | js2n.Nullable std.number.Integer
                            | optional,
                          tcpSocket
                            | js2n.Nullable {
                              host
                                | js2n.Nullable String
                                | optional,
                              port
                                | js2n.OneOf [
                                  js2n.Nullable String,
                                  js2n.Nullable std.number.Integer
                                ]
                            }
                            | optional,
                          terminationGracePeriodSeconds
                            | js2n.Nullable std.number.Integer
                            | optional,
                          timeoutSeconds
                            | js2n.Nullable std.number.Integer
                            | optional
                        }
                        | optional,
                      name | String,
                      ports
                        | js2n.Nullable (
                          Array (
                            js2n.Nullable {
                              containerPort | std.number.Integer,
                              hostIP
                                | js2n.Nullable String
                                | optional,
                              hostPort
                                | js2n.Nullable std.number.Integer
                                | optional,
                              name
                                | js2n.Nullable String
                                | optional,
                              protocol
                                | js2n.Nullable String
                                | optional
                            }
                          )
                        )
                        | optional,
                      readinessProbe
                        | js2n.Nullable {
                          exec
                            | js2n.Nullable {
                              command
                                | js2n.Nullable (Array (js2n.Nullable String))
                                | optional
                            }
                            | optional,
                          failureThreshold
                            | js2n.Nullable std.number.Integer
                            | optional,
                          grpc
                            | js2n.Nullable {
                              port | std.number.Integer,
                              service
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          httpGet
                            | js2n.Nullable {
                              host
                                | js2n.Nullable String
                                | optional,
                              httpHeaders
                                | js2n.Nullable (
                                  Array (
                                    js2n.Nullable {
                                      name | String,
                                      value | String
                                    }
                                  )
                                )
                                | optional,
                              path
                                | js2n.Nullable String
                                | optional,
                              port
                                | js2n.OneOf [
                                  js2n.Nullable String,
                                  js2n.Nullable std.number.Integer
                                ],
                              scheme
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          initialDelaySeconds
                            | js2n.Nullable std.number.Integer
                            | optional,
                          periodSeconds
                            | js2n.Nullable std.number.Integer
                            | optional,
                          successThreshold
                            | js2n.Nullable std.number.Integer
                            | optional,
                          tcpSocket
                            | js2n.Nullable {
                              host
                                | js2n.Nullable String
                                | optional,
                              port
                                | js2n.OneOf [
                                  js2n.Nullable String,
                                  js2n.Nullable std.number.Integer
                                ]
                            }
                            | optional,
                          terminationGracePeriodSeconds
                            | js2n.Nullable std.number.Integer
                            | optional,
                          timeoutSeconds
                            | js2n.Nullable std.number.Integer
                            | optional
                        }
                        | optional,
                      resizePolicy
                        | js2n.Nullable (
                          Array (
                            js2n.Nullable {
                              resourceName | String,
                              restartPolicy | String
                            }
                          )
                        )
                        | optional,
                      resources
                        | js2n.Nullable {
                          claims
                            | js2n.Nullable (
                              Array (
                                js2n.Nullable {
                                  name | String,
                                  request
                                    | js2n.Nullable String
                                    | optional
                                }
                              )
                            )
                            | optional,
                          limits
                            | js2n.Nullable (
                              {
                                _ | js2n.OneOf [
                                  js2n.Nullable String,
                                  js2n.Nullable Number
                                ]
                              }
                            )
                            | optional,
                          requests
                            | js2n.Nullable (
                              {
                                _ | js2n.OneOf [
                                  js2n.Nullable String,
                                  js2n.Nullable Number
                                ]
                              }
                            )
                            | optional
                        }
                        | optional,
                      restartPolicy
                        | js2n.Nullable String
                        | optional,
                      restartPolicyRules
                        | js2n.Nullable (
                          Array (
                            js2n.Nullable {
                              action | String,
                              exitCodes
                                | js2n.Nullable {
                                  operator | String,
                                  values
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable std.number.Integer
                                      )
                                    )
                                    | optional
                                }
                                | optional
                            }
                          )
                        )
                        | optional,
                      securityContext
                        | js2n.Nullable {
                          allowPrivilegeEscalation
                            | js2n.Nullable Bool
                            | optional,
                          appArmorProfile
                            | js2n.Nullable {
                              localhostProfile
                                | js2n.Nullable String
                                | optional,
                              type | String
                            }
                            | optional,
                          capabilities
                            | js2n.Nullable {
                              add
                                | js2n.Nullable (Array (js2n.Nullable String))
                                | optional,
                              drop
                                | js2n.Nullable (Array (js2n.Nullable String))
                                | optional
                            }
                            | optional,
                          privileged
                            | js2n.Nullable Bool
                            | optional,
                          procMount
                            | js2n.Nullable String
                            | optional,
                          readOnlyRootFilesystem
                            | js2n.Nullable Bool
                            | optional,
                          runAsGroup
                            | js2n.Nullable std.number.Integer
                            | optional,
                          runAsNonRoot
                            | js2n.Nullable Bool
                            | optional,
                          runAsUser
                            | js2n.Nullable std.number.Integer
                            | optional,
                          seLinuxOptions
                            | js2n.Nullable {
                              level
                                | js2n.Nullable String
                                | optional,
                              role
                                | js2n.Nullable String
                                | optional,
                              type
                                | js2n.Nullable String
                                | optional,
                              user
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          seccompProfile
                            | js2n.Nullable {
                              localhostProfile
                                | js2n.Nullable String
                                | optional,
                              type | String
                            }
                            | optional,
                          windowsOptions
                            | js2n.Nullable {
                              gmsaCredentialSpec
                                | js2n.Nullable String
                                | optional,
                              gmsaCredentialSpecName
                                | js2n.Nullable String
                                | optional,
                              hostProcess
                                | js2n.Nullable Bool
                                | optional,
                              runAsUserName
                                | js2n.Nullable String
                                | optional
                            }
                            | optional
                        }
                        | optional,
                      startupProbe
                        | js2n.Nullable {
                          exec
                            | js2n.Nullable {
                              command
                                | js2n.Nullable (Array (js2n.Nullable String))
                                | optional
                            }
                            | optional,
                          failureThreshold
                            | js2n.Nullable std.number.Integer
                            | optional,
                          grpc
                            | js2n.Nullable {
                              port | std.number.Integer,
                              service
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          httpGet
                            | js2n.Nullable {
                              host
                                | js2n.Nullable String
                                | optional,
                              httpHeaders
                                | js2n.Nullable (
                                  Array (
                                    js2n.Nullable {
                                      name | String,
                                      value | String
                                    }
                                  )
                                )
                                | optional,
                              path
                                | js2n.Nullable String
                                | optional,
                              port
                                | js2n.OneOf [
                                  js2n.Nullable String,
                                  js2n.Nullable std.number.Integer
                                ],
                              scheme
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          initialDelaySeconds
                            | js2n.Nullable std.number.Integer
                            | optional,
                          periodSeconds
                            | js2n.Nullable std.number.Integer
                            | optional,
                          successThreshold
                            | js2n.Nullable std.number.Integer
                            | optional,
                          tcpSocket
                            | js2n.Nullable {
                              host
                                | js2n.Nullable String
                                | optional,
                              port
                                | js2n.OneOf [
                                  js2n.Nullable String,
                                  js2n.Nullable std.number.Integer
                                ]
                            }
                            | optional,
                          terminationGracePeriodSeconds
                            | js2n.Nullable std.number.Integer
                            | optional,
                          timeoutSeconds
                            | js2n.Nullable std.number.Integer
                            | optional
                        }
                        | optional,
                      stdin | js2n.Nullable Bool | optional,
                      stdinOnce | js2n.Nullable Bool | optional,
                      targetContainerName
                        | js2n.Nullable String
                        | optional,
                      terminationMessagePath
                        | js2n.Nullable String
                        | optional,
                      terminationMessagePolicy
                        | js2n.Nullable String
                        | optional,
                      tty | js2n.Nullable Bool | optional,
                      volumeDevices
                        | js2n.Nullable (
                          Array (
                            js2n.Nullable { devicePath | String, name | String }
                          )
                        )
                        | optional,
                      volumeMounts
                        | js2n.Nullable (
                          Array (
                            js2n.Nullable {
                              mountPath | String,
                              mountPropagation
                                | js2n.Nullable String
                                | optional,
                              name | String,
                              readOnly
                                | js2n.Nullable Bool
                                | optional,
                              recursiveReadOnly
                                | js2n.Nullable String
                                | optional,
                              subPath
                                | js2n.Nullable String
                                | optional,
                              subPathExpr
                                | js2n.Nullable String
                                | optional
                            }
                          )
                        )
                        | optional,
                      workingDir | js2n.Nullable String | optional
                    }
                  )
                )
                | optional,
              hostAliases
                | js2n.Nullable (
                  Array (
                    js2n.Nullable {
                      hostnames
                        | js2n.Nullable (Array (js2n.Nullable String))
                        | optional,
                      ip | String
                    }
                  )
                )
                | optional,
              hostIPC | js2n.Nullable Bool | optional,
              hostNetwork | js2n.Nullable Bool | optional,
              hostPID | js2n.Nullable Bool | optional,
              hostUsers | js2n.Nullable Bool | optional,
              hostname | js2n.Nullable String | optional,
              hostnameOverride | js2n.Nullable String | optional,
              imagePullSecrets
                | js2n.Nullable (
                  Array (
                    js2n.Nullable { name | js2n.Nullable String | optional }
                  )
                )
                | optional,
              initContainers
                | js2n.Nullable (
                  Array (
                    js2n.Nullable {
                      args
                        | js2n.Nullable (Array (js2n.Nullable String))
                        | optional,
                      command
                        | js2n.Nullable (Array (js2n.Nullable String))
                        | optional,
                      env
                        | js2n.Nullable (
                          Array (
                            js2n.Nullable {
                              name | String,
                              value
                                | js2n.Nullable String
                                | optional,
                              valueFrom
                                | js2n.Nullable {
                                  configMapKeyRef
                                    | js2n.Nullable {
                                      key | String,
                                      name
                                        | js2n.Nullable String
                                        | optional,
                                      "optional"
                                        | js2n.Nullable Bool
                                        | optional
                                    }
                                    | optional,
                                  fieldRef
                                    | js2n.Nullable {
                                      apiVersion
                                        | js2n.Nullable String
                                        | optional,
                                      fieldPath | String
                                    }
                                    | optional,
                                  fileKeyRef
                                    | js2n.Nullable {
                                      key | String,
                                      "optional"
                                        | js2n.Nullable Bool
                                        | optional,
                                      path | String,
                                      volumeName | String
                                    }
                                    | optional,
                                  resourceFieldRef
                                    | js2n.Nullable {
                                      containerName
                                        | js2n.Nullable String
                                        | optional,
                                      divisor
                                        | js2n.OneOf [
                                          js2n.Nullable String,
                                          js2n.Nullable Number
                                        ]
                                        | optional,
                                      resource | String
                                    }
                                    | optional,
                                  secretKeyRef
                                    | js2n.Nullable {
                                      key | String,
                                      name
                                        | js2n.Nullable String
                                        | optional,
                                      "optional"
                                        | js2n.Nullable Bool
                                        | optional
                                    }
                                    | optional
                                }
                                | optional
                            }
                          )
                        )
                        | optional,
                      envFrom
                        | js2n.Nullable (
                          Array (
                            js2n.Nullable {
                              configMapRef
                                | js2n.Nullable {
                                  name
                                    | js2n.Nullable String
                                    | optional,
                                  "optional"
                                    | js2n.Nullable Bool
                                    | optional
                                }
                                | optional,
                              prefix
                                | js2n.Nullable String
                                | optional,
                              secretRef
                                | js2n.Nullable {
                                  name
                                    | js2n.Nullable String
                                    | optional,
                                  "optional"
                                    | js2n.Nullable Bool
                                    | optional
                                }
                                | optional
                            }
                          )
                        )
                        | optional,
                      image | js2n.Nullable String | optional,
                      imagePullPolicy
                        | js2n.Nullable String
                        | optional,
                      lifecycle
                        | js2n.Nullable {
                          postStart
                            | js2n.Nullable {
                              exec
                                | js2n.Nullable {
                                  command
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable String
                                      )
                                    )
                                    | optional
                                }
                                | optional,
                              httpGet
                                | js2n.Nullable {
                                  host
                                    | js2n.Nullable String
                                    | optional,
                                  httpHeaders
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable {
                                          name | String,
                                          value | String
                                        }
                                      )
                                    )
                                    | optional,
                                  path
                                    | js2n.Nullable String
                                    | optional,
                                  port
                                    | js2n.OneOf [
                                      js2n.Nullable String,
                                      js2n.Nullable std.number.Integer
                                    ],
                                  scheme
                                    | js2n.Nullable String
                                    | optional
                                }
                                | optional,
                              sleep
                                | js2n.Nullable {
                                  seconds | std.number.Integer
                                }
                                | optional,
                              tcpSocket
                                | js2n.Nullable {
                                  host
                                    | js2n.Nullable String
                                    | optional,
                                  port
                                    | js2n.OneOf [
                                      js2n.Nullable String,
                                      js2n.Nullable std.number.Integer
                                    ]
                                }
                                | optional
                            }
                            | optional,
                          preStop
                            | js2n.Nullable {
                              exec
                                | js2n.Nullable {
                                  command
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable String
                                      )
                                    )
                                    | optional
                                }
                                | optional,
                              httpGet
                                | js2n.Nullable {
                                  host
                                    | js2n.Nullable String
                                    | optional,
                                  httpHeaders
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable {
                                          name | String,
                                          value | String
                                        }
                                      )
                                    )
                                    | optional,
                                  path
                                    | js2n.Nullable String
                                    | optional,
                                  port
                                    | js2n.OneOf [
                                      js2n.Nullable String,
                                      js2n.Nullable std.number.Integer
                                    ],
                                  scheme
                                    | js2n.Nullable String
                                    | optional
                                }
                                | optional,
                              sleep
                                | js2n.Nullable {
                                  seconds | std.number.Integer
                                }
                                | optional,
                              tcpSocket
                                | js2n.Nullable {
                                  host
                                    | js2n.Nullable String
                                    | optional,
                                  port
                                    | js2n.OneOf [
                                      js2n.Nullable String,
                                      js2n.Nullable std.number.Integer
                                    ]
                                }
                                | optional
                            }
                            | optional,
                          stopSignal
                            | js2n.Nullable String
                            | optional
                        }
                        | optional,
                      livenessProbe
                        | js2n.Nullable {
                          exec
                            | js2n.Nullable {
                              command
                                | js2n.Nullable (Array (js2n.Nullable String))
                                | optional
                            }
                            | optional,
                          failureThreshold
                            | js2n.Nullable std.number.Integer
                            | optional,
                          grpc
                            | js2n.Nullable {
                              port | std.number.Integer,
                              service
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          httpGet
                            | js2n.Nullable {
                              host
                                | js2n.Nullable String
                                | optional,
                              httpHeaders
                                | js2n.Nullable (
                                  Array (
                                    js2n.Nullable {
                                      name | String,
                                      value | String
                                    }
                                  )
                                )
                                | optional,
                              path
                                | js2n.Nullable String
                                | optional,
                              port
                                | js2n.OneOf [
                                  js2n.Nullable String,
                                  js2n.Nullable std.number.Integer
                                ],
                              scheme
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          initialDelaySeconds
                            | js2n.Nullable std.number.Integer
                            | optional,
                          periodSeconds
                            | js2n.Nullable std.number.Integer
                            | optional,
                          successThreshold
                            | js2n.Nullable std.number.Integer
                            | optional,
                          tcpSocket
                            | js2n.Nullable {
                              host
                                | js2n.Nullable String
                                | optional,
                              port
                                | js2n.OneOf [
                                  js2n.Nullable String,
                                  js2n.Nullable std.number.Integer
                                ]
                            }
                            | optional,
                          terminationGracePeriodSeconds
                            | js2n.Nullable std.number.Integer
                            | optional,
                          timeoutSeconds
                            | js2n.Nullable std.number.Integer
                            | optional
                        }
                        | optional,
                      name | String,
                      ports
                        | js2n.Nullable (
                          Array (
                            js2n.Nullable {
                              containerPort | std.number.Integer,
                              hostIP
                                | js2n.Nullable String
                                | optional,
                              hostPort
                                | js2n.Nullable std.number.Integer
                                | optional,
                              name
                                | js2n.Nullable String
                                | optional,
                              protocol
                                | js2n.Nullable String
                                | optional
                            }
                          )
                        )
                        | optional,
                      readinessProbe
                        | js2n.Nullable {
                          exec
                            | js2n.Nullable {
                              command
                                | js2n.Nullable (Array (js2n.Nullable String))
                                | optional
                            }
                            | optional,
                          failureThreshold
                            | js2n.Nullable std.number.Integer
                            | optional,
                          grpc
                            | js2n.Nullable {
                              port | std.number.Integer,
                              service
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          httpGet
                            | js2n.Nullable {
                              host
                                | js2n.Nullable String
                                | optional,
                              httpHeaders
                                | js2n.Nullable (
                                  Array (
                                    js2n.Nullable {
                                      name | String,
                                      value | String
                                    }
                                  )
                                )
                                | optional,
                              path
                                | js2n.Nullable String
                                | optional,
                              port
                                | js2n.OneOf [
                                  js2n.Nullable String,
                                  js2n.Nullable std.number.Integer
                                ],
                              scheme
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          initialDelaySeconds
                            | js2n.Nullable std.number.Integer
                            | optional,
                          periodSeconds
                            | js2n.Nullable std.number.Integer
                            | optional,
                          successThreshold
                            | js2n.Nullable std.number.Integer
                            | optional,
                          tcpSocket
                            | js2n.Nullable {
                              host
                                | js2n.Nullable String
                                | optional,
                              port
                                | js2n.OneOf [
                                  js2n.Nullable String,
                                  js2n.Nullable std.number.Integer
                                ]
                            }
                            | optional,
                          terminationGracePeriodSeconds
                            | js2n.Nullable std.number.Integer
                            | optional,
                          timeoutSeconds
                            | js2n.Nullable std.number.Integer
                            | optional
                        }
                        | optional,
                      resizePolicy
                        | js2n.Nullable (
                          Array (
                            js2n.Nullable {
                              resourceName | String,
                              restartPolicy | String
                            }
                          )
                        )
                        | optional,
                      resources
                        | js2n.Nullable {
                          claims
                            | js2n.Nullable (
                              Array (
                                js2n.Nullable {
                                  name | String,
                                  request
                                    | js2n.Nullable String
                                    | optional
                                }
                              )
                            )
                            | optional,
                          limits
                            | js2n.Nullable (
                              {
                                _ | js2n.OneOf [
                                  js2n.Nullable String,
                                  js2n.Nullable Number
                                ]
                              }
                            )
                            | optional,
                          requests
                            | js2n.Nullable (
                              {
                                _ | js2n.OneOf [
                                  js2n.Nullable String,
                                  js2n.Nullable Number
                                ]
                              }
                            )
                            | optional
                        }
                        | optional,
                      restartPolicy
                        | js2n.Nullable String
                        | optional,
                      restartPolicyRules
                        | js2n.Nullable (
                          Array (
                            js2n.Nullable {
                              action | String,
                              exitCodes
                                | js2n.Nullable {
                                  operator | String,
                                  values
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable std.number.Integer
                                      )
                                    )
                                    | optional
                                }
                                | optional
                            }
                          )
                        )
                        | optional,
                      securityContext
                        | js2n.Nullable {
                          allowPrivilegeEscalation
                            | js2n.Nullable Bool
                            | optional,
                          appArmorProfile
                            | js2n.Nullable {
                              localhostProfile
                                | js2n.Nullable String
                                | optional,
                              type | String
                            }
                            | optional,
                          capabilities
                            | js2n.Nullable {
                              add
                                | js2n.Nullable (Array (js2n.Nullable String))
                                | optional,
                              drop
                                | js2n.Nullable (Array (js2n.Nullable String))
                                | optional
                            }
                            | optional,
                          privileged
                            | js2n.Nullable Bool
                            | optional,
                          procMount
                            | js2n.Nullable String
                            | optional,
                          readOnlyRootFilesystem
                            | js2n.Nullable Bool
                            | optional,
                          runAsGroup
                            | js2n.Nullable std.number.Integer
                            | optional,
                          runAsNonRoot
                            | js2n.Nullable Bool
                            | optional,
                          runAsUser
                            | js2n.Nullable std.number.Integer
                            | optional,
                          seLinuxOptions
                            | js2n.Nullable {
                              level
                                | js2n.Nullable String
                                | optional,
                              role
                                | js2n.Nullable String
                                | optional,
                              type
                                | js2n.Nullable String
                                | optional,
                              user
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          seccompProfile
                            | js2n.Nullable {
                              localhostProfile
                                | js2n.Nullable String
                                | optional,
                              type | String
                            }
                            | optional,
                          windowsOptions
                            | js2n.Nullable {
                              gmsaCredentialSpec
                                | js2n.Nullable String
                                | optional,
                              gmsaCredentialSpecName
                                | js2n.Nullable String
                                | optional,
                              hostProcess
                                | js2n.Nullable Bool
                                | optional,
                              runAsUserName
                                | js2n.Nullable String
                                | optional
                            }
                            | optional
                        }
                        | optional,
                      startupProbe
                        | js2n.Nullable {
                          exec
                            | js2n.Nullable {
                              command
                                | js2n.Nullable (Array (js2n.Nullable String))
                                | optional
                            }
                            | optional,
                          failureThreshold
                            | js2n.Nullable std.number.Integer
                            | optional,
                          grpc
                            | js2n.Nullable {
                              port | std.number.Integer,
                              service
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          httpGet
                            | js2n.Nullable {
                              host
                                | js2n.Nullable String
                                | optional,
                              httpHeaders
                                | js2n.Nullable (
                                  Array (
                                    js2n.Nullable {
                                      name | String,
                                      value | String
                                    }
                                  )
                                )
                                | optional,
                              path
                                | js2n.Nullable String
                                | optional,
                              port
                                | js2n.OneOf [
                                  js2n.Nullable String,
                                  js2n.Nullable std.number.Integer
                                ],
                              scheme
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          initialDelaySeconds
                            | js2n.Nullable std.number.Integer
                            | optional,
                          periodSeconds
                            | js2n.Nullable std.number.Integer
                            | optional,
                          successThreshold
                            | js2n.Nullable std.number.Integer
                            | optional,
                          tcpSocket
                            | js2n.Nullable {
                              host
                                | js2n.Nullable String
                                | optional,
                              port
                                | js2n.OneOf [
                                  js2n.Nullable String,
                                  js2n.Nullable std.number.Integer
                                ]
                            }
                            | optional,
                          terminationGracePeriodSeconds
                            | js2n.Nullable std.number.Integer
                            | optional,
                          timeoutSeconds
                            | js2n.Nullable std.number.Integer
                            | optional
                        }
                        | optional,
                      stdin | js2n.Nullable Bool | optional,
                      stdinOnce | js2n.Nullable Bool | optional,
                      terminationMessagePath
                        | js2n.Nullable String
                        | optional,
                      terminationMessagePolicy
                        | js2n.Nullable String
                        | optional,
                      tty | js2n.Nullable Bool | optional,
                      volumeDevices
                        | js2n.Nullable (
                          Array (
                            js2n.Nullable { devicePath | String, name | String }
                          )
                        )
                        | optional,
                      volumeMounts
                        | js2n.Nullable (
                          Array (
                            js2n.Nullable {
                              mountPath | String,
                              mountPropagation
                                | js2n.Nullable String
                                | optional,
                              name | String,
                              readOnly
                                | js2n.Nullable Bool
                                | optional,
                              recursiveReadOnly
                                | js2n.Nullable String
                                | optional,
                              subPath
                                | js2n.Nullable String
                                | optional,
                              subPathExpr
                                | js2n.Nullable String
                                | optional
                            }
                          )
                        )
                        | optional,
                      workingDir | js2n.Nullable String | optional
                    }
                  )
                )
                | optional,
              nodeName | js2n.Nullable String | optional,
              nodeSelector
                | js2n.Nullable ({ _ | js2n.Nullable String })
                | optional,
              os | js2n.Nullable { name | String } | optional,
              overhead
                | js2n.Nullable (
                  {
                    _ | js2n.OneOf [js2n.Nullable String, js2n.Nullable Number ]
                  }
                )
                | optional,
              preemptionPolicy | js2n.Nullable String | optional,
              "priority"
                | js2n.Nullable std.number.Integer
                | optional,
              priorityClassName | js2n.Nullable String | optional,
              readinessGates
                | js2n.Nullable (Array (js2n.Nullable { conditionType | String }))
                | optional,
              resourceClaims
                | js2n.Nullable (
                  Array (
                    js2n.Nullable {
                      name | String,
                      resourceClaimName
                        | js2n.Nullable String
                        | optional,
                      resourceClaimTemplateName
                        | js2n.Nullable String
                        | optional
                    }
                  )
                )
                | optional,
              resources
                | js2n.Nullable {
                  claims
                    | js2n.Nullable (
                      Array (
                        js2n.Nullable {
                          name | String,
                          request
                            | js2n.Nullable String
                            | optional
                        }
                      )
                    )
                    | optional,
                  limits
                    | js2n.Nullable (
                      {
                        _ | js2n.OneOf [
                          js2n.Nullable String,
                          js2n.Nullable Number
                        ]
                      }
                    )
                    | optional,
                  requests
                    | js2n.Nullable (
                      {
                        _ | js2n.OneOf [
                          js2n.Nullable String,
                          js2n.Nullable Number
                        ]
                      }
                    )
                    | optional
                }
                | optional,
              restartPolicy | js2n.Nullable String | optional,
              runtimeClassName | js2n.Nullable String | optional,
              schedulerName | js2n.Nullable String | optional,
              schedulingGates
                | js2n.Nullable (Array (js2n.Nullable { name | String }))
                | optional,
              securityContext
                | js2n.Nullable {
                  appArmorProfile
                    | js2n.Nullable {
                      localhostProfile
                        | js2n.Nullable String
                        | optional,
                      type | String
                    }
                    | optional,
                  fsGroup
                    | js2n.Nullable std.number.Integer
                    | optional,
                  fsGroupChangePolicy
                    | js2n.Nullable String
                    | optional,
                  runAsGroup
                    | js2n.Nullable std.number.Integer
                    | optional,
                  runAsNonRoot | js2n.Nullable Bool | optional,
                  runAsUser
                    | js2n.Nullable std.number.Integer
                    | optional,
                  seLinuxChangePolicy
                    | js2n.Nullable String
                    | optional,
                  seLinuxOptions
                    | js2n.Nullable {
                      level | js2n.Nullable String | optional,
                      role | js2n.Nullable String | optional,
                      type | js2n.Nullable String | optional,
                      user | js2n.Nullable String | optional
                    }
                    | optional,
                  seccompProfile
                    | js2n.Nullable {
                      localhostProfile
                        | js2n.Nullable String
                        | optional,
                      type | String
                    }
                    | optional,
                  supplementalGroups
                    | js2n.Nullable (Array (js2n.Nullable std.number.Integer))
                    | optional,
                  supplementalGroupsPolicy
                    | js2n.Nullable String
                    | optional,
                  sysctls
                    | js2n.Nullable (
                      Array (
                        js2n.Nullable { name | String, value | String }
                      )
                    )
                    | optional,
                  windowsOptions
                    | js2n.Nullable {
                      gmsaCredentialSpec
                        | js2n.Nullable String
                        | optional,
                      gmsaCredentialSpecName
                        | js2n.Nullable String
                        | optional,
                      hostProcess | js2n.Nullable Bool | optional,
                      runAsUserName
                        | js2n.Nullable String
                        | optional
                    }
                    | optional
                }
                | optional,
              serviceAccount | js2n.Nullable String | optional,
              serviceAccountName | js2n.Nullable String | optional,
              setHostnameAsFQDN | js2n.Nullable Bool | optional,
              shareProcessNamespace | js2n.Nullable Bool | optional,
              subdomain | js2n.Nullable String | optional,
              terminationGracePeriodSeconds
                | js2n.Nullable std.number.Integer
                | optional,
              tolerations
                | js2n.Nullable (
                  Array (
                    js2n.Nullable {
                      effect | js2n.Nullable String | optional,
                      key | js2n.Nullable String | optional,
                      operator | js2n.Nullable String | optional,
                      tolerationSeconds
                        | js2n.Nullable std.number.Integer
                        | optional,
                      value | js2n.Nullable String | optional
                    }
                  )
                )
                | optional,
              topologySpreadConstraints
                | js2n.Nullable (
                  Array (
                    js2n.Nullable {
                      labelSelector
                        | js2n.Nullable {
                          matchExpressions
                            | js2n.Nullable (
                              Array (
                                js2n.Nullable {
                                  key | String,
                                  operator | String,
                                  values
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable String
                                      )
                                    )
                                    | optional
                                }
                              )
                            )
                            | optional,
                          matchLabels
                            | js2n.Nullable ({ _ | js2n.Nullable String })
                            | optional
                        }
                        | optional,
                      matchLabelKeys
                        | js2n.Nullable (Array (js2n.Nullable String))
                        | optional,
                      maxSkew | std.number.Integer,
                      minDomains
                        | js2n.Nullable std.number.Integer
                        | optional,
                      nodeAffinityPolicy
                        | js2n.Nullable String
                        | optional,
                      nodeTaintsPolicy
                        | js2n.Nullable String
                        | optional,
                      topologyKey | String,
                      whenUnsatisfiable | String
                    }
                  )
                )
                | optional,
              volumes
                | js2n.Nullable (
                  Array (
                    js2n.Nullable {
                      awsElasticBlockStore
                        | js2n.Nullable {
                          fsType
                            | js2n.Nullable String
                            | optional,
                          partition
                            | js2n.Nullable std.number.Integer
                            | optional,
                          readOnly
                            | js2n.Nullable Bool
                            | optional,
                          volumeID | String
                        }
                        | optional,
                      azureDisk
                        | js2n.Nullable {
                          cachingMode
                            | js2n.Nullable String
                            | optional,
                          diskName | String,
                          diskURI | String,
                          fsType
                            | js2n.Nullable String
                            | optional,
                          kind | js2n.Nullable String | optional,
                          readOnly | js2n.Nullable Bool | optional
                        }
                        | optional,
                      azureFile
                        | js2n.Nullable {
                          readOnly
                            | js2n.Nullable Bool
                            | optional,
                          secretName | String,
                          shareName | String
                        }
                        | optional,
                      cephfs
                        | js2n.Nullable {
                          monitors | Array (js2n.Nullable String),
                          path | js2n.Nullable String | optional,
                          readOnly
                            | js2n.Nullable Bool
                            | optional,
                          secretFile
                            | js2n.Nullable String
                            | optional,
                          secretRef
                            | js2n.Nullable {
                              name
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          user | js2n.Nullable String | optional
                        }
                        | optional,
                      cinder
                        | js2n.Nullable {
                          fsType
                            | js2n.Nullable String
                            | optional,
                          readOnly
                            | js2n.Nullable Bool
                            | optional,
                          secretRef
                            | js2n.Nullable {
                              name
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          volumeID | String
                        }
                        | optional,
                      configMap
                        | js2n.Nullable {
                          defaultMode
                            | js2n.Nullable std.number.Integer
                            | optional,
                          items
                            | js2n.Nullable (
                              Array (
                                js2n.Nullable {
                                  key | String,
                                  mode
                                    | js2n.Nullable std.number.Integer
                                    | optional,
                                  path | String
                                }
                              )
                            )
                            | optional,
                          name | js2n.Nullable String | optional,
                          "optional"
                            | js2n.Nullable Bool
                            | optional
                        }
                        | optional,
                      csi
                        | js2n.Nullable {
                          driver | String,
                          fsType
                            | js2n.Nullable String
                            | optional,
                          nodePublishSecretRef
                            | js2n.Nullable {
                              name
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          readOnly
                            | js2n.Nullable Bool
                            | optional,
                          volumeAttributes
                            | js2n.Nullable ({ _ | js2n.Nullable String })
                            | optional
                        }
                        | optional,
                      downwardAPI
                        | js2n.Nullable {
                          defaultMode
                            | js2n.Nullable std.number.Integer
                            | optional,
                          items
                            | js2n.Nullable (
                              Array (
                                js2n.Nullable {
                                  fieldRef
                                    | js2n.Nullable {
                                      apiVersion
                                        | js2n.Nullable String
                                        | optional,
                                      fieldPath | String
                                    }
                                    | optional,
                                  mode
                                    | js2n.Nullable std.number.Integer
                                    | optional,
                                  path | String,
                                  resourceFieldRef
                                    | js2n.Nullable {
                                      containerName
                                        | js2n.Nullable String
                                        | optional,
                                      divisor
                                        | js2n.OneOf [
                                          js2n.Nullable String,
                                          js2n.Nullable Number
                                        ]
                                        | optional,
                                      resource | String
                                    }
                                    | optional
                                }
                              )
                            )
                            | optional
                        }
                        | optional,
                      emptyDir
                        | js2n.Nullable {
                          medium
                            | js2n.Nullable String
                            | optional,
                          sizeLimit
                            | js2n.OneOf [
                              js2n.Nullable String,
                              js2n.Nullable Number
                            ]
                            | optional
                        }
                        | optional,
                      ephemeral
                        | js2n.Nullable {
                          volumeClaimTemplate
                            | js2n.Nullable {
                              metadata
                                | js2n.Nullable {
                                  annotations
                                    | js2n.Nullable (
                                      {
                                        _ | js2n.Nullable String
                                      }
                                    )
                                    | optional,
                                  creationTimestamp
                                    | js2n.Nullable String
                                    | optional,
                                  deletionGracePeriodSeconds
                                    | js2n.Nullable std.number.Integer
                                    | optional,
                                  deletionTimestamp
                                    | js2n.Nullable String
                                    | optional,
                                  finalizers
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable String
                                      )
                                    )
                                    | optional,
                                  generateName
                                    | js2n.Nullable String
                                    | optional,
                                  generation
                                    | js2n.Nullable std.number.Integer
                                    | optional,
                                  labels
                                    | js2n.Nullable (
                                      {
                                        _ | js2n.Nullable String
                                      }
                                    )
                                    | optional,
                                  managedFields
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable {
                                          apiVersion
                                            | js2n.Nullable String
                                            | optional,
                                          fieldsType
                                            | js2n.Nullable String
                                            | optional,
                                          fieldsV1
                                            | js2n.Nullable ({ _ | Dyn })
                                            | optional,
                                          manager
                                            | js2n.Nullable String
                                            | optional,
                                          operation
                                            | js2n.Nullable String
                                            | optional,
                                          subresource
                                            | js2n.Nullable String
                                            | optional,
                                          time
                                            | js2n.Nullable String
                                            | optional
                                        }
                                      )
                                    )
                                    | optional,
                                  name
                                    | js2n.Nullable String
                                    | optional,
                                  namespace
                                    | js2n.Nullable String
                                    | optional,
                                  ownerReferences
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable {
                                          apiVersion
                                            | String,
                                          blockOwnerDeletion
                                            | js2n.Nullable Bool
                                            | optional,
                                          controller
                                            | js2n.Nullable Bool
                                            | optional,
                                          kind | String,
                                          name | String,
                                          uid | String
                                        }
                                      )
                                    )
                                    | optional,
                                  resourceVersion
                                    | js2n.Nullable String
                                    | optional,
                                  selfLink
                                    | js2n.Nullable String
                                    | optional,
                                  uid
                                    | js2n.Nullable String
                                    | optional
                                }
                                | optional,
                              spec
                                | {
                                  accessModes
                                    | js2n.Nullable (
                                      Array (
                                        js2n.Nullable String
                                      )
                                    )
                                    | optional,
                                  dataSource
                                    | js2n.Nullable {
                                      apiGroup
                                        | js2n.Nullable String
                                        | optional,
                                      kind | String,
                                      name | String
                                    }
                                    | optional,
                                  dataSourceRef
                                    | js2n.Nullable {
                                      apiGroup
                                        | js2n.Nullable String
                                        | optional,
                                      kind | String,
                                      name | String,
                                      namespace
                                        | js2n.Nullable String
                                        | optional
                                    }
                                    | optional,
                                  resources
                                    | js2n.Nullable {
                                      limits
                                        | js2n.Nullable (
                                          {
                                            _ | js2n.OneOf [
                                              js2n.Nullable String,
                                              js2n.Nullable Number
                                            ]
                                          }
                                        )
                                        | optional,
                                      requests
                                        | js2n.Nullable (
                                          {
                                            _ | js2n.OneOf [
                                              js2n.Nullable String,
                                              js2n.Nullable Number
                                            ]
                                          }
                                        )
                                        | optional
                                    }
                                    | optional,
                                  selector
                                    | js2n.Nullable {
                                      matchExpressions
                                        | js2n.Nullable (
                                          Array (
                                            js2n.Nullable {
                                              key | String,
                                              operator
                                                | String,
                                              values
                                                | js2n.Nullable (
                                                  Array (
                                                    js2n.Nullable String
                                                  )
                                                )
                                                | optional
                                            }
                                          )
                                        )
                                        | optional,
                                      matchLabels
                                        | js2n.Nullable (
                                          {
                                            _ | js2n.Nullable String
                                          }
                                        )
                                        | optional
                                    }
                                    | optional,
                                  storageClassName
                                    | js2n.Nullable String
                                    | optional,
                                  volumeAttributesClassName
                                    | js2n.Nullable String
                                    | optional,
                                  volumeMode
                                    | js2n.Nullable String
                                    | optional,
                                  volumeName
                                    | js2n.Nullable String
                                    | optional
                                }
                            }
                            | optional
                        }
                        | optional,
                      fc
                        | js2n.Nullable {
                          fsType
                            | js2n.Nullable String
                            | optional,
                          lun
                            | js2n.Nullable std.number.Integer
                            | optional,
                          readOnly
                            | js2n.Nullable Bool
                            | optional,
                          targetWWNs
                            | js2n.Nullable (Array (js2n.Nullable String))
                            | optional,
                          wwids
                            | js2n.Nullable (Array (js2n.Nullable String))
                            | optional
                        }
                        | optional,
                      flexVolume
                        | js2n.Nullable {
                          driver | String,
                          fsType
                            | js2n.Nullable String
                            | optional,
                          options
                            | js2n.Nullable ({ _ | js2n.Nullable String })
                            | optional,
                          readOnly
                            | js2n.Nullable Bool
                            | optional,
                          secretRef
                            | js2n.Nullable {
                              name
                                | js2n.Nullable String
                                | optional
                            }
                            | optional
                        }
                        | optional,
                      flocker
                        | js2n.Nullable {
                          datasetName
                            | js2n.Nullable String
                            | optional,
                          datasetUUID
                            | js2n.Nullable String
                            | optional
                        }
                        | optional,
                      gcePersistentDisk
                        | js2n.Nullable {
                          fsType
                            | js2n.Nullable String
                            | optional,
                          partition
                            | js2n.Nullable std.number.Integer
                            | optional,
                          pdName | String,
                          readOnly | js2n.Nullable Bool | optional
                        }
                        | optional,
                      gitRepo
                        | js2n.Nullable {
                          directory
                            | js2n.Nullable String
                            | optional,
                          repository | String,
                          revision
                            | js2n.Nullable String
                            | optional
                        }
                        | optional,
                      glusterfs
                        | js2n.Nullable {
                          endpoints | String,
                          path | String,
                          readOnly | js2n.Nullable Bool | optional
                        }
                        | optional,
                      hostPath
                        | js2n.Nullable {
                          path | String,
                          type | js2n.Nullable String | optional
                        }
                        | optional,
                      image
                        | js2n.Nullable {
                          pullPolicy
                            | js2n.Nullable String
                            | optional,
                          reference
                            | js2n.Nullable String
                            | optional
                        }
                        | optional,
                      iscsi
                        | js2n.Nullable {
                          chapAuthDiscovery
                            | js2n.Nullable Bool
                            | optional,
                          chapAuthSession
                            | js2n.Nullable Bool
                            | optional,
                          fsType
                            | js2n.Nullable String
                            | optional,
                          initiatorName
                            | js2n.Nullable String
                            | optional,
                          iqn | String,
                          iscsiInterface
                            | js2n.Nullable String
                            | optional,
                          lun | std.number.Integer,
                          portals
                            | js2n.Nullable (Array (js2n.Nullable String))
                            | optional,
                          readOnly
                            | js2n.Nullable Bool
                            | optional,
                          secretRef
                            | js2n.Nullable {
                              name
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          targetPortal | String
                        }
                        | optional,
                      name | String,
                      nfs
                        | js2n.Nullable {
                          path | String,
                          readOnly
                            | js2n.Nullable Bool
                            | optional,
                          server | String
                        }
                        | optional,
                      persistentVolumeClaim
                        | js2n.Nullable {
                          claimName | String,
                          readOnly | js2n.Nullable Bool | optional
                        }
                        | optional,
                      photonPersistentDisk
                        | js2n.Nullable {
                          fsType
                            | js2n.Nullable String
                            | optional,
                          pdID | String
                        }
                        | optional,
                      portworxVolume
                        | js2n.Nullable {
                          fsType
                            | js2n.Nullable String
                            | optional,
                          readOnly
                            | js2n.Nullable Bool
                            | optional,
                          volumeID | String
                        }
                        | optional,
                      projected
                        | js2n.Nullable {
                          defaultMode
                            | js2n.Nullable std.number.Integer
                            | optional,
                          sources
                            | js2n.Nullable (
                              Array (
                                js2n.Nullable {
                                  clusterTrustBundle
                                    | js2n.Nullable {
                                      labelSelector
                                        | js2n.Nullable {
                                          matchExpressions
                                            | js2n.Nullable (
                                              Array (
                                                js2n.Nullable {
                                                  key
                                                    | String,
                                                  operator
                                                    | String,
                                                  values
                                                    | js2n.Nullable (
                                                      Array (
                                                        js2n.Nullable String
                                                      )
                                                    )
                                                    | optional
                                                }
                                              )
                                            )
                                            | optional,
                                          matchLabels
                                            | js2n.Nullable (
                                              {
                                                _ | js2n.Nullable String
                                              }
                                            )
                                            | optional
                                        }
                                        | optional,
                                      name
                                        | js2n.Nullable String
                                        | optional,
                                      "optional"
                                        | js2n.Nullable Bool
                                        | optional,
                                      path | String,
                                      signerName
                                        | js2n.Nullable String
                                        | optional
                                    }
                                    | optional,
                                  configMap
                                    | js2n.Nullable {
                                      items
                                        | js2n.Nullable (
                                          Array (
                                            js2n.Nullable {
                                              key | String,
                                              mode
                                                | js2n.Nullable std.number.Integer
                                                | optional,
                                              path | String
                                            }
                                          )
                                        )
                                        | optional,
                                      name
                                        | js2n.Nullable String
                                        | optional,
                                      "optional"
                                        | js2n.Nullable Bool
                                        | optional
                                    }
                                    | optional,
                                  downwardAPI
                                    | js2n.Nullable {
                                      items
                                        | js2n.Nullable (
                                          Array (
                                            js2n.Nullable {
                                              fieldRef
                                                | js2n.Nullable {
                                                  apiVersion
                                                    | js2n.Nullable String
                                                    | optional,
                                                  fieldPath
                                                    | String
                                                }
                                                | optional,
                                              mode
                                                | js2n.Nullable std.number.Integer
                                                | optional,
                                              path | String,
                                              resourceFieldRef
                                                | js2n.Nullable {
                                                  containerName
                                                    | js2n.Nullable String
                                                    | optional,
                                                  divisor
                                                    | js2n.OneOf [
                                                      js2n.Nullable String,
                                                      js2n.Nullable Number
                                                    ]
                                                    | optional,
                                                  resource
                                                    | String
                                                }
                                                | optional
                                            }
                                          )
                                        )
                                        | optional
                                    }
                                    | optional,
                                  podCertificate
                                    | js2n.Nullable {
                                      certificateChainPath
                                        | js2n.Nullable String
                                        | optional,
                                      credentialBundlePath
                                        | js2n.Nullable String
                                        | optional,
                                      keyPath
                                        | js2n.Nullable String
                                        | optional,
                                      keyType | String,
                                      maxExpirationSeconds
                                        | js2n.Nullable std.number.Integer
                                        | optional,
                                      signerName | String
                                    }
                                    | optional,
                                  secret
                                    | js2n.Nullable {
                                      items
                                        | js2n.Nullable (
                                          Array (
                                            js2n.Nullable {
                                              key | String,
                                              mode
                                                | js2n.Nullable std.number.Integer
                                                | optional,
                                              path | String
                                            }
                                          )
                                        )
                                        | optional,
                                      name
                                        | js2n.Nullable String
                                        | optional,
                                      "optional"
                                        | js2n.Nullable Bool
                                        | optional
                                    }
                                    | optional,
                                  serviceAccountToken
                                    | js2n.Nullable {
                                      audience
                                        | js2n.Nullable String
                                        | optional,
                                      expirationSeconds
                                        | js2n.Nullable std.number.Integer
                                        | optional,
                                      path | String
                                    }
                                    | optional
                                }
                              )
                            )
                            | optional
                        }
                        | optional,
                      quobyte
                        | js2n.Nullable {
                          group | js2n.Nullable String | optional,
                          readOnly
                            | js2n.Nullable Bool
                            | optional,
                          registry | String,
                          tenant
                            | js2n.Nullable String
                            | optional,
                          user | js2n.Nullable String | optional,
                          volume | String
                        }
                        | optional,
                      rbd
                        | js2n.Nullable {
                          fsType
                            | js2n.Nullable String
                            | optional,
                          image | String,
                          keyring
                            | js2n.Nullable String
                            | optional,
                          monitors | Array (js2n.Nullable String),
                          pool | js2n.Nullable String | optional,
                          readOnly
                            | js2n.Nullable Bool
                            | optional,
                          secretRef
                            | js2n.Nullable {
                              name
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          user | js2n.Nullable String | optional
                        }
                        | optional,
                      scaleIO
                        | js2n.Nullable {
                          fsType
                            | js2n.Nullable String
                            | optional,
                          gateway | String,
                          protectionDomain
                            | js2n.Nullable String
                            | optional,
                          readOnly
                            | js2n.Nullable Bool
                            | optional,
                          secretRef
                            | {
                              name
                                | js2n.Nullable String
                                | optional
                            },
                          sslEnabled
                            | js2n.Nullable Bool
                            | optional,
                          storageMode
                            | js2n.Nullable String
                            | optional,
                          storagePool
                            | js2n.Nullable String
                            | optional,
                          system | String,
                          volumeName
                            | js2n.Nullable String
                            | optional
                        }
                        | optional,
                      secret
                        | js2n.Nullable {
                          defaultMode
                            | js2n.Nullable std.number.Integer
                            | optional,
                          items
                            | js2n.Nullable (
                              Array (
                                js2n.Nullable {
                                  key | String,
                                  mode
                                    | js2n.Nullable std.number.Integer
                                    | optional,
                                  path | String
                                }
                              )
                            )
                            | optional,
                          "optional"
                            | js2n.Nullable Bool
                            | optional,
                          secretName
                            | js2n.Nullable String
                            | optional
                        }
                        | optional,
                      storageos
                        | js2n.Nullable {
                          fsType
                            | js2n.Nullable String
                            | optional,
                          readOnly
                            | js2n.Nullable Bool
                            | optional,
                          secretRef
                            | js2n.Nullable {
                              name
                                | js2n.Nullable String
                                | optional
                            }
                            | optional,
                          volumeName
                            | js2n.Nullable String
                            | optional,
                          volumeNamespace
                            | js2n.Nullable String
                            | optional
                        }
                        | optional,
                      vsphereVolume
                        | js2n.Nullable {
                          fsType
                            | js2n.Nullable String
                            | optional,
                          storagePolicyID
                            | js2n.Nullable String
                            | optional,
                          storagePolicyName
                            | js2n.Nullable String
                            | optional,
                          volumePath | String
                        }
                        | optional
                    }
                  )
                )
                | optional
            }
            | optional
        }
        | optional
    }
    | optional,
  status
    | js2n.Nullable {
      availableReplicas | js2n.Nullable std.number.Integer | optional,
      conditions
        | js2n.Nullable (
          Array (
            js2n.Nullable {
              lastTransitionTime | js2n.Nullable String | optional,
              message | js2n.Nullable String | optional,
              reason | js2n.Nullable String | optional,
              status | String,
              type | String
            }
          )
        )
        | optional,
      fullyLabeledReplicas | js2n.Nullable std.number.Integer | optional,
      observedGeneration | js2n.Nullable std.number.Integer | optional,
      readyReplicas | js2n.Nullable std.number.Integer | optional,
      replicas | std.number.Integer
    }
    | optional
}
