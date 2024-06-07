{
  # Internal operations. Can't be accessed from user code because `$` is not a
  # valid starting character for an identifier.

  # Builtin contract implementations
  "$dyn" = fun _label value => value,

  "$num" = fun label value => if %typeof% value == 'Number then value else %blame% label,

  "$bool" = fun label value => if %typeof% value == 'Bool then value else %blame% label,

  "$string" = fun label value => if %typeof% value == 'String then value else %blame% label,

  "$foreign_id" = fun label value => if %typeof% value == 'ForeignId then value else %blame% label,

  "$fail" = fun label _value => %blame% label,

  "$array" = fun element_contract label value =>
    if %typeof% value == 'Array then
      %contract/array_lazy_apply% (%label/go_array% label) value element_contract
    else
      %blame% label,

  # A specialized version of `$array $dyn`, but which is constant time.
  "$array_dyn" = fun label value =>
    if %typeof% value == 'Array then
      value
    else
      %blame% label,

  "$func" = fun domain codomain label value =>
    if %typeof% value == 'Function then
      (
        fun x =>
          %contract/apply%
            codomain
            (%label/go_codom% label)
            (value (%contract/apply% domain (%label/flip_polarity% (%label/go_dom% label)) x))
      )
    else
      %blame% label,

  # A specialied version of `_ -> Dyn`
  "$func_dom" = fun domain label value =>
    if %typeof% value == 'Function then
      (
        fun x =>
          value
            (
              %contract/apply%
                domain
                (%label/flip_polarity% (%label/go_dom% label))
                x
            )
      )
    else
      %blame% label,

  # A specialied version of `Dyn -> _`
  "$func_codom" = fun codomain label value =>
    if %typeof% value == 'Function then
      (
        fun x =>
          %contract/apply%
            codomain
            (%label/go_codom% label)
            (value x)
      )
    else
      %blame% label,

  # A specialied version of `Dyn -> Dyn`
  "$func_dyn" = fun label value =>
    if %typeof% value == 'Function then
      value
    else
      %blame% label,

  "$forall_var" = fun sealing_key label value =>
    let current_polarity = %label/polarity% label in
    let polarity = (%label/lookup_type_variable% sealing_key label).polarity in
    if polarity == current_polarity then
      %unseal% sealing_key value (%blame% label)
    else
      # [^forall_chng_pol]: Blame assignment for polymorphic contracts
      # should take into account the polarity at the point the forall was
      # introduced, not the current polarity of the variable occurrence. Indeed,
      # forall can never blame in a negative position (relative to the
      # forall): the contract is entirely on the callee.
      #
      # Thus, for correct blame assignment, we want to set the polarity to the
      # forall polarity (here `polarity`). Because we only have the `chng_pol`
      # primop, and we know that in this branch they are unequal, flipping the
      # current polarity will indeed give the original forall's polarity.
      %seal% sealing_key (%label/flip_polarity% label) value,

  "$forall" = fun sealing_key polarity contract label value =>
    contract (%label/insert_type_variable% sealing_key polarity label) value,

  "$enum" = fun case label value =>
    if %typeof% value == 'Enum then
      %contract/apply% case label value
    else
      %blame% (%label/with_message% "expected an enum" label),

  "$enum_fail" = fun label =>
    %blame% (%label/with_message% "tag not in the enum type" label),

  # Contract for an enum variant with tag `'tag`, that is any value of the form
  # `'tag exp`.
  "$enum_variant" = fun tag label value =>
    if %enum/is_variant% value then
      let value_tag = %enum/get_tag% value in

      if value_tag == tag then
        value
      else
        let msg = "expected `'%{%to_string% tag}`, got `'%{%to_string% value_tag}`" in
        %blame% (%label/with_message% "tag mismatch: %{msg}" label)
    else
      %blame% (%label/with_message% "expected an enum variant" label),

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
    value,

  "$record" = fun field_contracts tail_contract label value =>
    if %typeof% value == 'Record then
      # Returns the sub-record of `left` containing only those fields which are not
      # present in `right`. If `left` has a sealed polymorphic tail then it will be
      # preserved.
      let field_diff = fun left right =>
        std.array.fold_left
          (
            fun acc field =>
              if %record/has_field% field right then
                acc
              else
                %record/insert% field acc (left."%{field}")
          )
          (%record/empty_with_tail% left)
          (%record/fields% left)
      in
      let contracts_not_in_value = field_diff field_contracts value in
      let missing_fields = %record/fields% contracts_not_in_value in
      if %array/length% missing_fields == 0 then
        let tail_fields = field_diff value field_contracts in
        let fields_with_contracts =
          std.array.fold_left
            (
              fun acc field =>
                if %record/has_field% field field_contracts then
                  let contract = field_contracts."%{field}" in
                  let label = %label/go_field% field label in
                  let val = value."%{field}" in
                  %record/insert% field acc (%contract/apply% contract label val)
                else
                  acc
            )
            {}
            (%record/fields% value)
        in
        tail_contract fields_with_contracts label tail_fields
      else
        let plural = if %array/length% missing_fields == 1 then "" else "s" in
        %blame%
          (
            %label/with_message%
              "missing field%{plural} `%{std.string.join ", " missing_fields}`"
              label
          )
    else
      %blame% (%label/with_message% "not a record" label),

  # Lazy dictionary contract for `{_ | T}`
  "$dict_contract" = fun contract label value =>
    if %typeof% value == 'Record then
      %contract/record_lazy_apply% (%label/go_dict% label) value (fun _field => contract)
    else
      %blame% (%label/with_message% "not a record" label),

  # Eager dictionary contract for `{_ : T}`
  "$dict_type" = fun contract label value =>
    if %typeof% value == 'Record then
      %record/map%
        value
        (
          fun _field field_value =>
            %contract/apply% contract (%label/go_dict% label) field_value
        )
    else
      %blame% (%label/with_message% "not a record" label),

  # A specialized version of either `{_ | Dyn}` or `{_ : Dyn}` (which are
  # equivalent), but which is constant time.
  "$dict_dyn" = fun label value =>
    if %typeof% value == 'Record then
      value
    else
      %blame% (%label/with_message% "not a record" label),

  "$forall_record_tail" = fun sealing_key constr acc label value =>
    let current_polarity = %label/polarity% label in
    let polarity = (%label/lookup_type_variable% sealing_key label).polarity in
    let plural = fun list => if %array/length% list == 1 then "" else "s" in
    if polarity == current_polarity then
      if value == {} then
        let tagged_label = %label/with_message% "polymorphic tail mismatch" label in
        let tail = %record/unseal_tail% sealing_key tagged_label value in
        acc & tail
      else
        let extra_fields = %record/fields% value in
        %blame%
          (
            %label/with_message%
              "extra field%{plural extra_fields} `%{std.string.join ", " extra_fields}`"
              label
          )
    else
      let conflicts =
        std.array.filter
          (fun field => std.array.elem field constr)
          (%record/fields% value)
      in
      if conflicts != [] then
        %blame%
          (
            %label/with_message%
              "field%{plural conflicts} not allowed in tail: `%{std.string.join ", " conflicts}`"
              label
          )
      else
        # See [^forall_chng_pol]
        %record/seal_tail% sealing_key (%label/flip_polarity% label) acc value,

  "$dyn_tail" = fun acc label value => acc & value,

  "$empty_tail" = fun acc label value =>
    if value == {} then
      acc
    else
      let extra_fields = %record/fields% value in
      let plural = if %array/length% extra_fields == 1 then "" else "s" in
      %blame%
        (
          %label/with_message%
            "extra field%{plural} `%{std.string.join ", " extra_fields}`"
            label
        ),

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
