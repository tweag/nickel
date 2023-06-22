{
  # Internal operations. Can't be accessed from user code because `$` is not a
  # valid starting character for an identifier.

  # Contract implementations
  "$dyn" = fun _label value => value,

  "$num" = fun label value => if %typeof% value == 'Number then value else %blame% label,

  "$bool" = fun label value => if %typeof% value == 'Bool then value else %blame% label,

  "$string" = fun label value => if %typeof% value == 'String then value else %blame% label,

  "$fail" = fun label _value => %blame% label,

  "$array" = fun element_contract label value =>
    if %typeof% value == 'Array then
      %array_lazy_assume% (%go_array% label) value element_contract
    else
      %blame% label,

  "$func" = fun domain codomain label value =>
    if %typeof% value == 'Function then
      (fun x => %assume% codomain (%go_codom% label) (value (%assume% domain (%chng_pol% (%go_dom% label)) x)))
    else
      %blame% label,

  "$forall_var" = fun sealing_key label value =>
    let current_polarity = %polarity% label in
    let polarity = (%lookup_type_variable% sealing_key label).polarity in
    if polarity == current_polarity then
      %unseal% sealing_key value (%blame% label)
    else
      # Here, we know that this term should be sealed, but to give the right
      # blame for the contract, we have to change the polarity to match the
      # polarity of the `Forall`, because this is what's important for
      # blaming polymorphic contracts.
      %seal% sealing_key (%chng_pol% label) value,

  "$forall" =
    let flip = match {
      'Positive => 'Negative,
      'Negative => 'Positive,
    }
    in
    fun sealing_key polarity contract label value =>
      let polarity = if %dualize% label then flip polarity else polarity in
      contract (%insert_type_variable% sealing_key polarity label) value,

  "$enums" = fun case label value =>
    if %typeof% value == 'Enum then
      %assume% case label value
    else
      %blame% (%label_with_message% "not an enum tag" label),

  "$enum_fail" = fun label =>
    %blame% (%label_with_message% "tag not included in the enum type" label),

  "$record" = fun field_contracts tail_contract label value =>
    if %typeof% value == 'Record then
      # Returns the sub-record of `left` containing only those fields which are not
      # present in `right`. If `left` has a sealed polymorphic tail then it will be
      # preserved.
      let field_diff = fun left right =>
        std.array.fold_left
          (
            fun acc field =>
              if %has_field% field right then
                acc
              else
                %record_insert% field acc (left."%{field}")
          )
          (%record_empty_with_tail% left)
          (%fields% left)
      in
      let contracts_not_in_value = field_diff field_contracts value in
      let missing_fields = %fields% contracts_not_in_value in
      if %length% missing_fields == 0 then
        let tail_fields = field_diff value field_contracts in
        let fields_with_contracts =
          std.array.fold_left
            (
              fun acc field =>
                if %has_field% field field_contracts then
                  let contract = field_contracts."%{field}" in
                  let label = %go_field% field label in
                  let val = value."%{field}" in
                  %record_insert% field acc (%assume% contract label val)
                else
                  acc
            )
            {}
            (%fields% value)
        in
        tail_contract fields_with_contracts label tail_fields
      else
        let plural = if %length% missing_fields == 1 then "" else "s" in
        %blame%
          (
            %label_with_message%
              "missing field%{plural} `%{std.string.join ", " missing_fields}`"
              label
          )
    else
      %blame% (%label_with_message% "not a record" label),

  # Lazy dictionary contract for `{_ | T}`
  "$dict_contract" = fun contract label value =>
    if %typeof% value == 'Record then
      %record_lazy_assume% (%go_dict% label) value (fun _field => contract)
    else
      %blame% (%label_with_message% "not a record" label),

  # Eager dictionary contract for `{_ : T}`
  "$dict_type" = fun contract label value =>
    if %typeof% value == 'Record then
      %record_map%
        value
        (
          fun _field field_value =>
            %assume% contract (%go_dict% label) field_value
        )
    else
      %blame% (%label_with_message% "not a record" label),

  "$forall_tail" = fun sealing_key constr acc label value =>
    let current_polarity = %polarity% label in
    let polarity = (%lookup_type_variable% sealing_key label).polarity in
    let plural = fun list => if %length% list == 1 then "" else "s" in
    if polarity == current_polarity then
      if value == {} then
        let tagged_label = %label_with_message% "polymorphic tail mismatch" label in
        let tail = %record_unseal_tail% sealing_key tagged_label value in
        acc & tail
      else
        let extra_fields = %fields% value in
        %blame%
          (
            %label_with_message%
              "extra field%{plural extra_fields} `%{std.string.join ", " extra_fields}`"
              label
          )
    else
      let conflicts =
        std.array.filter
          (fun field => std.array.elem field constr)
          (%fields% value)
      in
      if conflicts != [] then
        %blame%
          (
            %label_with_message%
              "field%{plural conflicts} not allowed in tail: `%{std.string.join ", " conflicts}`"
              label
          )
      else
        # Note: in order to correctly attribute blame, the polarity of `l`
        # must match the polarity of the `forall` which introduced the
        # polymorphic contract (i.e. `pol`). Since we know in this branch
        # that `pol` and `%polarity% l` differ, we swap `l`'s polarity before
        # we continue.
        %record_seal_tail% sealing_key (%chng_pol% label) acc value,

  "$dyn_tail" = fun acc label value => acc & value,

  "$empty_tail" = fun acc label value =>
    if value == {} then
      acc
    else
      let extra_fields = %fields% value in
      let plural = if %length% extra_fields == 1 then "" else "s" in
      %blame%
        (
          %label_with_message%
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
