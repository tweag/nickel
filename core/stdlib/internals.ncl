{
  # Internal operations. Can't be accessed from user code because `$` is not a
  # valid starting character for an identifier.

  # Builtin contract implementations

  "$dyn/immediate" = fun _value => 'Ok,

  "$number/immediate" = fun value => if %typeof% value == 'Number then 'Ok else 'Error {},

  "$bool/immediate" = fun value => if %typeof% value == 'Bool then 'Ok else 'Error {},

  "$string/immediate" = fun value => if %typeof% value == 'String then 'Ok else 'Error {},

  "$foreign_id/immediate" = fun value =>
    if %typeof% value == 'ForeignId then 'Ok else 'Error {},

  "$fail/immediate" = fun _value => 'Error {},

  "$array/immediate" = fun value =>
    if %typeof% value == 'Array then 'Ok else 'Error {},

  "$array/delayed" = fun ElemContract label value =>
    %contract/array_lazy_apply% (%label/go_array% label) value ElemContract,

  # A specialized version of `$array $dyn`, but which is constant time.
  "$array_dyn/immediate" = fun value =>
    if %typeof% value == 'Array then
      'Ok
    else
      'Error {},

  "$func/immediate" = fun value =>
    if %typeof% value == 'Function then
      'Ok
    else
      'Error {},

  "$func/delayed" = fun Domain Codomain label value =>
    fun x =>
      %contract/apply%
        Codomain
        (%label/go_codom% label)
        (
          value
            (
              %contract/apply%
                Domain
                (%label/flip_polarity% (%label/go_dom% label))
                x
            )
        ),

  # A specialied version of `_ -> Dyn`
  "$func_dom/delayed" = fun domain label value =>
    fun x =>
      value
        (
          %contract/apply%
            domain
            (%label/flip_polarity% (%label/go_dom% label))
            x
        ),

  # A specialied version of `Dyn -> _`
  "$func_codom/delayed" = fun Codomain label value =>
    fun x =>
      %contract/apply%
        Codomain
        (%label/go_codom% label)
        (value x),

  "$forall_var" = fun sealing_key label value =>
    let current_polarity = %label/polarity% label in
    let polarity = (%label/lookup_type_variable% sealing_key label).polarity in
    if polarity == current_polarity then
      %unseal% sealing_key value (%blame% label)
    else
      # [^forall_label_flip_polarity]: Blame assignment for polymorphic
      # contracts should take into account the polarity at the point the forall
      # was introduced, not the current polarity of the variable occurrence.
      # Indeed, forall can never blame in a negative position (relative to the
      # forall): the contract is entirely on the callee.
      #
      # Thus, for correct blame assignment, we want to set the polarity to the
      # forall polarity (here `polarity`). Because we only have the
      # `%label/flip_polarity%` primop, and we know that in this branch they are
      # unequal, flipping the current polarity will indeed give the original
      # forall's polarity.
      %seal% sealing_key (%label/flip_polarity% label) value,

  # Contracts for a `forall a. T` type. Mostly a wrapper around `T` that
  # properly sets additional metadata for polymorphic (un)sealing.
  # The immediate part just forwards to the immediate part of `T`.
  "$forall/immediate" = fun contract => (%contract/get_immediate% contract),
  "$forall/delayed" = fun sealing_key polarity contract label =>
    (%contract/get_delayed% contract)
      (%label/insert_type_variable% sealing_key polarity label),

  "$enum_fail" = 'Error { message = "wrong enum tag" },

  "$enum/immediate" = fun matcher value =>
    if %typeof% value == 'Enum then
      matcher value
    else
      'Error { message = "expected an enum" },

  "$enum/delayed" = fun payload label value =>
    payload
    |> match {
      'Map Contract =>
        # it's basically
        #
        # ```nickel
        # std.enum.map
        #  (std.contract.apply Contract label)
        #  value
        # ```
        #
        # but reimplemented directly to avoid stdlib references, which are always a
        # pain to handle in `$internals` as members of this module must be able to
        # run correctly in an arbitrary environment, which could shadow stdlib
        # members.
        #
        # Since `std.enum.map` is short, it's simpler to just inline it here.
        %enum/make_variant%
          (%to_string% (%enum/get_tag% value))
          (%contract/apply% Contract label (%enum/get_arg% value)),
      # If there's no contract provided by the immediate part, we are in the
      # tail case, where we need to apply a potential sealing/unsealing operation
      # implemented by the enum variant's argument.
      'Tail tail_contract =>
        tail_contract label value,
    },

  # Contract for an enum variant with tag `'tag`, that is any value of the form
  # `'tag exp`.
  "$enum_variant/immediate" = fun tag value =>
    if %enum/is_variant% value then
      let value_tag = %enum/get_tag% value in

      if value_tag == tag then
        'Ok
      else
        'Error {
          message = "tag mismatch",
          notes = [
            "Expected tag `'%{%to_string% tag}`, got tag `'%{%to_string% value_tag}`"
          ]
        }
    else
      'Error { message = "expected an enum variant" },

  "$forall_enum_tail" = fun label value =>
    # Theoretically, we should seal/unseal values that are part of enum tail
    # and `$forall_enum_tail` should be defined similarly to
    # `$forall_record_tail`, as a function of `sealing_key` as well.
    #
    # However, we can't just do that, because then a match expression that is
    # entirely legit, for example
    #
    # ```
    # match { 'Foo => 1, _ => 2 } : forall r. [| 'Foo; r|] -> Number`
    # ```
    #
    # would fail on `'Bar`  because it's sealed. It looks like we should allow
    # `match` to see through sealed enum, but proceed only if the final
    # catch-all case matches what's inside the sealed enum, and not a more
    # precise parametricity-breaking pattern.
    #
    # Unfortunately, that would break the current stdlib because parametricity
    # has never been enforced correctly for enum types in the past. For example,
    # `std.string.from_enum` has contract `forall a. [|; a |] -> String` which
    # does violate parametricity, as it looks inside its argument although it's
    # part of a polymorphic tail.
    #
    # While this might be an issue to investigate in the longer term, or for
    # the next major version, we continue to just not enforce parametricity
    # for enum types for now to maintain backward-compatibility.
    value,

  # An immediate contract that simply checks that a value has the type 'Record.
  "$record/immediate" = fun value =>
    if %typeof% value == 'Record then
      'Ok
    else
      'Error { message = "expected a record" },

  # The immediate part of the contract for static record types.
  #
  # Takes as additional parameters a record mapping field names to their type
  # and a boolean indicating if the record type has a tail, which in practice
  # allows for extra fields.
  #
  # The immediate part checks that there are no missing fields, and that there
  # are no extra fields if there is no tail. Then, it passes the computed split
  # of the dictionary of contracts and the provided record value to the delayed
  # part as a payload.
  "$record_type/immediate" =
    let plural = fun list => if %array/length% list == 1 then "" else "s" in

    fun field_contracts has_tail label value =>
      if %typeof% value == 'Record then
        let split_result = %record/split_pair% field_contracts value in

        if split_result.left_only != {} then
          let missing_fields = %record/fields% split_result.left_only in

          'Error {
            message = "missing field%{plural missing_fields} `%{std.string.join ", " missing_fields}`",
          }
        else if split_result.right_only != {} && !has_tail then
          let extra_fields = %record/fields% split_result.right_only in

          'Error {
            message = "extra field%{plural extra_fields} `%{std.string.join ", " extra_fields}`",
          }
        else
          'Ok { payload = split_result }
      else
        'Error { message = "expected a record" },

  # The delayed part of the contract for static record types.
  #
  # Takes as additional parameters a record mapping field names to their type
  # and a helper handling the tail sealing/unsealing.
  #
  # The delayed part applies the contracts for each field (through record
  # mapping) and then applies the tail contract to potential extra fields that
  # might require sealing.
  "$record_type/delayed" = fun field_contracts tail_contract payload label value =>
    let extra_fields = payload.right_only in
    let fields_with_contracts =
      %record/map%
        (
          fun field value =>
            %contract/apply%
              field_contracts."%{field}"
              (%label/go_field% field label)
              value
        )
        payload.right_center
    in

    tail_contract extra_fields label fields_with_contracts,

  # Delayed part of the Lazy dictionary contract for `{_ | T}`. It's lazy in the
  # sense that the contracts for fields are pushed inside each fields' metadata,
  # where they are stored but applied lazily.
  "$dict_contract/delayed" = fun contract label value =>
    %contract/record_lazy_apply%
      (%label/go_dict% label)
      value
      (fun _field => contract),

  # Delayed part of the eager dictionary contract for `{_ : T}`. It's not eager
  # in the sense that the field contracts are run immediately - they are not.
  # However, as opposed to `$dict_contract`, the contract is mapped onto the
  # content of each field directly using record mapping, instead of being pushed
  # inside fields' metadata.
  "$dict_type/delayed" = fun contract label value =>
    %record/map%
      value
      (
        fun _field field_value =>
          %contract/apply% contract (%label/go_dict% label) field_value
      ),

  # Implementation of a helper for a polymorphic record tail.
  "$forall_record_tail" = fun sealing_key constr extra_fields label value =>
    let current_polarity = %label/polarity% label in
    let polarity = (%label/lookup_type_variable% sealing_key label).polarity in
    let plural = fun list => if %array/length% list == 1 then "" else "s" in

    # If the polarity of the forall is the same as the current polarity, we are
    # in a positive occurrence (relative to the forall). Think of a return value
    # of a function type. In this case, the tail is supposed to be sealed, and
    # the function isn't allowed to inject any extra field in that sealed tail.
    if polarity == current_polarity then
      if extra_fields == {} then
        let tagged_label = %label/with_message% "polymorphic tail mismatch" label in
        let tail = %record/unseal_tail% sealing_key tagged_label extra_fields in
        %record/disjoint_merge% value tail
      else
        let extra_fields = %record/fields% extra_fields in
        %blame%
          (
            %label/with_message%
              "extra field%{plural extra_fields} `%{std.string.join ", " extra_fields}`"
              label
          )
      # Otherwise, we are in a negative occurrence (relative to the forall). Think
      # of the argument of a function. There, extra fields are allowed, but must
      # be sealed in the tail to prevent the function from touching them.
    else
      # Conflicts happen because a polymorphic record tail might have additional
      # constraints as to what can actually be in there. See the documentation
      # of the typechecker for more information.
      let conflicts =
        std.array.filter
          (fun field => std.array.elem field constr)
          (%record/fields% extra_fields)
      in
      if conflicts != [] then
        %blame%
          (
            %label/with_message%
              "field%{plural conflicts} not allowed in tail: `%{std.string.join ", " conflicts}`"
              label
          )
      else
        # See [^forall_label_flip_polarity]
        %record/seal_tail% sealing_key (%label/flip_polarity% label) value extra_fields,

  # Implementation of a helper for a dynamic tail. A dynamic tail doesn't
  # seal anything, so we just add the extra fields back to the returned value.
  # See `$record_type/delayed`.
  "$dyn_tail" = fun extra_fields label value => %record/disjoint_merge% value extra_fields,

  # Implementation of a helper for an empty tail. At this point, we've already
  # checked (via `$record_type/immediate`) that there are no extra fields, so
  # we can just ignore them and return the value as is.
  "$empty_tail" = fun _extra_fields label value => value,

  # Take a contract as a predicate, built using %contract/from_predicate%, and
  # turn it into a generic custom contract (a partial identity Label -> Dyn ->
  # Dyn) that can be passed to %contract/apply%.
  "$predicate_to_ctr" = fun predicate label value =>
    if predicate value then
      value
    else
      %blame% label,

  # Take a contract as a validator, built using %contract/from_validator%, and
  # turn it into a generic custom contract (a partial identity Label -> Dyn ->
  # Dyn) that can be passed to %contract/apply%.
  "$validator_to_ctr" = fun validator label value =>
    validator value
    |> match {
      'Ok => value,
      'Error { message ? null, notes ? [], .. } =>
        let label =
          if message != null && std.is_string message then
            %label/with_message% message label
          else
            label
        in

        let label =
          if notes != [] && std.is_array notes then
            %label/with_notes% (%force% notes) label
          else
            label
        in

        %blame% label,
      # The contract of `std.contract.from_validator` should guarantee that we
      # never reach this case. However, nothing prevents user from using
      # `%contract/from_validator%` directly, so we still try to handle a
      # misbehaving validator gracefully.
      _ =>
        %blame%
          (
            %label/with_notes%
              (
                %force%
                  [
                    "The validator for this contract returned an invalid result (which must be either `'Ok` or `'Error {message, ..}`)",
                    "Please check the implementation of the contract that has been broken"
                  ]
              )
              label
          )
    },

  # Take a custom contract split as an immediate and delayed part, both of which
  # can be null, and transform it into a partial identity function that can be
  # given to %contract/apply%.
  "$prepare_contract" = fun contract label value =>
    let immediate = %contract/get_immediate% contract in
    let delayed = %contract/get_delayed% contract in

    if immediate != null then
      let result = immediate value in

      # In user code, we would have used a match construct here, which would be
      # more readable. However, internal functions are core utilities that get
      # called quite a lot (and internals should be fairly reduced), so we try
      # to use lower-level constructs to reclaim a tad more performance instead.
      if result == 'Done then
        value
      else if result == 'Ok then
        # Result can currently either be 'Ok or 'Ok { payload = ... }
        if %enum/is_variant% result then
          let payload = (%enum/get_arg% result).payload in
          if delayed != null then
            delayed payload label value
          else
            value
        else
          value
      else if %typeof% result == 'Enum && %enum/get_tag% result == 'Error then
        let payload = %enum/get_arg% result in

        let label =
          if %record/field_is_defined% "message" payload
          && std.is_string payload.message then
            %label/with_message% payload.message label
          else
            label
        in

        let label =
          if %record/field_is_defined% "notes" payload
          && std.is_array payload.notes then
            %label/with_notes% (%force% payload.notes) label
          else
            label
        in

        %blame% label
      else
        %blame%
          (
            %label/with_notes%
              (
                %force%
                  [
                    "The immediate part of this contract returned an invalid result (which must be either `'Ok`, `'Done`  or `'Error {..}`)",
                    "Please check the implementation of the contract that has been broken"
                  ]
              )
              label
          )
    else if delayed != null then
      delayed label value
    else
      value,

  # Recursive priorities operators

  "$rec_force" = fun value => %rec_force% (%force% value),
  "$rec_default" = fun value => %rec_default% (%force% value),

  # Provide access to std.contract.Equal within the initial environement. Merging
  # makes use of `std.contract.Equal`, but it can't blindly substitute such an
  # expression, because `contract` might have been redefined locally. Putting it
  # in an internal value prefixed with `$` makes it accessible from the initial
  # environment and prevents it from being shadowed.
  "$stdlib_contract_equal" = std.contract.Equal,
}
