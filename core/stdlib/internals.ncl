{
  # Internal operations. Can't be accessed from user code because `$` is not a
  # valid starting character for an identifier.

  # Builtin contract implementations. Note that these are wrapped in a
  # `CustomContract` constructor by the interpreter on the Rust side, so they
  # can be written without calling to `%contract/custom%`, but they still follow
  # the same convention (in particular the return type is [| 'Ok _, 'Error {..}
  # |]` as well.

  # Contract for the `Dyn` type. It's just an always accepting contract.
  "$dyn" = fun _label value => 'Ok value,

  # Contract for the `Number` type.
  "$num" = fun _label value =>
    if %typeof% value == 'Number then
      'Ok value
    else
      'Error {},

  # Contract for the `Bool` type.
  "$bool" = fun _label value =>
    if %typeof% value == 'Bool then
      'Ok value
    else
      'Error {},

  # Contract for the `String` type.
  "$string" = fun _label value =>
    if %typeof% value == 'String then
      'Ok value
    else
      'Error {},

  # Contract for `ForeignId` values.
  "$foreign_id" = fun _label value =>
    if %typeof% value == 'ForeignId then
      'Ok value
    else
      'Error {},

  # Contract for the `Array T` type, where `element_contract` is the contract
  # for `T`.
  "$array" = fun Element =>
    fun label value =>
      if %typeof% value == 'Array then
        'Ok (
          %contract/array_lazy_apply%
            (%label/go_array% label)
            value
            Element
        )
      else
        'Error { message = "expected an array" },

  # A specialized version of `$array $dyn`, but which is constant time.
  "$array_dyn" = fun _label value =>
    if %typeof% value == 'Array then
      'Ok value
    else
      'Error { message = "expected an array" },

  # Contract for the function type `Domain -> Codomain`.
  "$func" = fun Domain Codomain =>
    fun label value =>
      if %typeof% value == 'Function then
        'Ok (
          fun x =>
            %contract/apply%
              Codomain
              (%label/go_codom% label)
              (value (%contract/apply% Domain (%label/flip_polarity% (%label/go_dom% label)) x))
        )
      else
        'Error { message = "expected a function" },

  # A specialized version of `_ -> Dyn`
  "$func_dom" = fun Domain =>
    fun label value =>
      if %typeof% value == 'Function then
        'Ok (
          fun x =>
            value
              (
                %contract/apply%
                  Domain
                  (%label/flip_polarity% (%label/go_dom% label))
                  x
              )
        )
      else
        'Error { message = "expected a function" },

  # A specialied version of `Dyn -> _`
  "$func_codom" = fun Codomain =>
    fun label value =>
      if %typeof% value == 'Function then
        'Ok (
          fun x =>
            %contract/apply%
              Codomain
              (%label/go_codom% label)
              (value x)
        )
      else
        'Error { message = "expected a function" },

  # A specialied version of `Dyn -> Dyn`
  "$func_dyn" = fun _label value =>
    if %typeof% value == 'Function then
      'Ok value
    else
      'Error { message = "expected a function" },

  # Contract for a polymorphic variable. This contract is stored in an
  # environment during contract generation when a forall introduces a variable
  # and is inserted whenever this variable occurs somewhere in the contract of
  # the body of the forall.
  "$forall_var" = fun sealing_key =>
    fun label value =>
      let current_polarity = %label/polarity% label in
      let polarity = (%label/lookup_type_variable% sealing_key label).polarity in
      if polarity == current_polarity then
        'Ok (%unseal% sealing_key value (%blame% label))
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
        'Ok (%seal% sealing_key (%label/flip_polarity% label) value),

  # Contract for `forall a. T`, where `contract` is the contract for `T`. This
  # contract mostly set appropriate metadata in the label and forward the rest
  # to `T`.
  "$forall" = fun sealing_key polarity Contract label value =>
    let label = (%label/insert_type_variable% sealing_key polarity label) in
    %contract/check% Contract label value,

  # Contract for an enum type `[| 'tag1, 'tag2 arg, ... |]`. A match expression
  # depending on the structure of the enum type is generated by the interpreter
  # and provided through `matcher`, and does the bulk of the work. This contract
  # is just a wrapper that checks that the value is an enum and forwards the rest
  # to `matcher`.
  "$enum" = fun matcher =>
    fun label value =>
      if %typeof% value == 'Enum then
        matcher label value
      else
        'Error { message = "expected an enum" },

  # Error branch used when elaborating the matcher for an enum type. See
  # `$enum`.
  "$enum_fail" = fun _value => 'Error { message = "tag not in the enum type" },

  # Contract for an enum variant with tag `'tag`, that is any value of the form
  # `'tag exp`.
  "$enum_variant" = fun tag =>
    fun _label value =>
      if %enum/is_variant% value then
        let value_tag = %enum/get_tag% value in

        if value_tag == tag then
          'Ok value
        else
          'Error {
            message = "expected `'%{%to_string% tag}`, got `'%{%to_string% value_tag}`"
          }
      else
        'Error { message = "expected an enum variant" },

  # Tail wrapper for the tail of an enum type.
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
    # hasn't never been enforced correctly for enum types in the past. For
    # example, `std.string.from_enum` has contract
    # `forall a. [|; a |] -> String` which does violate parametricity, as it
    # looks inside its argument although it's part of a polymorphic tail.
    #
    # While this might be an issue to investigate in the longer term, or for
    # the next major version, we continue to just not enforce parametricity
    # for enum types for now to maintain backward-compatibility.
    'Ok value,

  # Record contract.
  #
  # Record contracts are written and manipulated as standard record literals by
  # the user, which is convenient (they can be written in a natural way and all
  # record operations apply on them, they can be merged, extended, accessed, etc.)
  #
  # Before being applied, they still need to be wrapped to provide a proper
  # error message when the checked value isn't a record, and to apply the proper
  # merge operator flavour.
  #
  # Note that `%record/contract_merge%` returns an immediate error as `'Error`
  # in case of extra fields. However, due to the lazy nature of merging and the
  # existence of field without a definition, missing fields are a delayed error.
  # This might have a non-obvious behavior when using boolean combinators on
  # record contracts.
  "$record_contract" = fun record_contract =>
    fun label value =>
      if %typeof% value == 'Record then
        %record/merge_contract% label value record_contract
      else
        'Error { message = "expected a Record" },

  # Contract for a static record type `{ field1: T1, field2: T2, ... }`.
  #
  # The record type is reified by the interpreter as a record value `{ field1 =
  # <contract of T1>, field2 = <contract of T2>, ... }` and given as the
  # `field_contracts` argument. Depending on the tail of the record type, the
  # corresponding `tail_wrapper` is also provided by the interpreter. Finally,
  # `has_tail` is a boolean indicating if the record type has a non-empty tail,
  # which makes it possible to push more checks in the immediate part of the
  # contract.
  "$record_type" =
    let plural = fun list => if %array/length% list == 1 then "" else "s" in

    fun field_contracts tail_wrapper has_tail =>
      fun label value =>
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
            let extra_fields = split_result.right_only in
            let fields_with_contracts =
              %record/map%
                split_result.right_center
                (
                  fun field value =>
                    %contract/apply%
                      field_contracts."%{field}"
                      (%label/go_field% field label)
                      value
                )
            in

            # Note that the tail_wrapper has the same type as a custom contract
            # and thus properly wraps the result in `'Ok`, so we can call it
            # directly
            tail_wrapper extra_fields label fields_with_contracts
        else
          'Error { message = "expected a record" },

  # Dictionary contract for `{_ | T}`. The contracts are mapped onto fields'
  # metadata.
  "$dict_contract" = fun Contract =>
    fun label value =>
      if %typeof% value == 'Record then
        'Ok (
          %contract/record_lazy_apply%
            (%label/go_dict% label)
            value
            (fun _field => Contract)
        )
      else
        'Error { message = "not a record" },

  # Dictionary contract for `{_ : T}`. The contracts are mapped directly onto
  # the fields' values, which makes it more eager (in some specific sense - this
  # contract is still lazy/delayed) than `$dict_contract`.
  "$dict_type" = fun Contract =>
    fun label value =>
      if %typeof% value == 'Record then
        'Ok (
          %record/map%
            value
            (
              fun _field field_value =>
                %contract/apply% Contract (%label/go_dict% label) field_value
            )
        )
      else
        'Error { message = "not a record" },

  # A specialized version of either `{_ | Dyn}` or `{_ : Dyn}` (which are
  # equivalent), but which is constant time.
  "$dict_dyn" = fun label value =>
    if %typeof% value == 'Record then
      'Ok value
    else
      'Error { message = "not a record" },

  # Tail wrapper for a polymorphic tail. Wrappers are used in the implementation
  # of the contract for static record types, and have the same return type as
  # other contract implementations.
  #
  # The `sealing_key` is the sealing key corresponding to the variable occuring
  # in the tail. `constr` are the fields that can't appear in the value because
  # they are already in the tail. Both are provided by the interpreter when
  # applying this function.
  #
  # `extra_fields` is provided by the caller `$record`, and is the set of fields
  # that are weren't part of the record type (thus are candidates for being in
  # the tail).
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

        'Ok (%record/disjoint_merge% value tail)
      else
        let extra_fields = %record/fields% extra_fields in

        'Error {
          message = "extra field%{plural extra_fields} `%{std.string.join ", " extra_fields}`",
        }
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
        'Error {
          message = "field%{plural conflicts} not allowed in tail: `%{std.string.join ", " conflicts}`",
        }
      else
        # See [^forall_label_flip_polarity]
        'Ok (
          %record/seal_tail%
            sealing_key
            (%label/flip_polarity% label)
            value
            extra_fields
        ),

  # Tail wrapper for a dynamic tail. A dynamic tail doesn't seal anything, so we
  # just add the extra fields back to the returned value. See
  # `$record_type/delayed`.
  "$dyn_tail" = fun extra_fields label value => 'Ok (%record/disjoint_merge% value extra_fields),

  # Tail wrapper of a helper for an empty tail. At this point, we've already
  # checked (in the main contract implementation for record types) that there
  # are no extra fields, so we can just ignore them and return the value as is.
  "$empty_tail" = fun _extra_fields _label value => 'Ok value,

  # Dual of prepare_custom_contract. Turns a naked function of a label and a
  # value to a custom contract-like representation (that is, a function
  # returning an enum - however the result is still a naked function, which must
  # be applicable as a normal function, and isn't wrapped in
  # `%contract/custom%`).
  "$naked_to_custom" = fun naked label value => 'Ok (naked label value),

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
