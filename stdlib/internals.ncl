{
  # Internal operations. Can't be accessed from user code because `$` is not a
  # valid starting character for an identifier.

  # Contract implementations
  "$dyn" = fun _label value => value,

  "$num" = fun label value => if %typeof% value == `Number then value else %blame% label,

  "$bool" = fun label value => if %typeof% value == `Bool then value else %blame% label,

  "$string" = fun label value => if %typeof% value == `String then value else %blame% label,

  "$fail" = fun label _value => %blame% label,

  "$array" = fun element-contract label value =>
    if %typeof% value == `Array then
        %array_lazy_assume% (%go_array% label) value element-contract
    else
        %blame% label,

  "$func" = fun domain codomain label value =>
      if %typeof% value == `Function then
          (fun x => %assume% codomain (%go_codom% label) (value (%assume% domain (%chng_pol% (%go_dom% label)) x)))
      else
          %blame% label,

  "$forall_var" = fun sealing-key polarity label value =>
      let current-polarity = %polarity% label in
      if polarity == current-polarity then
          %unseal% sealing-key value (%blame% label)
      else
          # Here, we know that this term should be sealed, but to give the right
          # blame for the contract, we have to change the polarity to match the
          # polarity of the `Forall`, because this is what's important for
          # blaming polymorphic contracts.
          %seal% sealing-key (%chng_pol% label) value,

  "$enums" = fun case label value =>
      if %typeof% value == `Enum then
          %assume% case label value
      else
          %blame% (%label_with_message% "not an enum tag" label),

  "$enum_fail" = fun label =>
      %blame% (%label_with_message% "tag not included in the enum type" label),

  "$record" = fun field_contracts tail_contract label value =>
    if %typeof% value == `Record then
      # Returns the sub-record of `l` containing only those fields which are not
      # present in `r`. If `l` has a sealed polymorphic tail then it will be
      # preserved.
      let field_diff = fun l r => array.fold_left
        (fun acc f =>
          if %has_field% f r then
            acc
          else
            %record_insert% f acc (l."%{f}"))
        (%record_empty_with_tail% l)
        (%fields% l)
      in

      let contracts_not_in_value = field_diff field_contracts value in
      let missing_fields = %fields% contracts_not_in_value in

      if %length% missing_fields == 0 then
        let tail_fields = field_diff value field_contracts in
        let fields_with_contracts = array.fold_left
          (fun acc f =>
            if %has_field% f field_contracts then
              let contract = field_contracts."%{f}" in
              let label = %go_field% f label in
              let val = value."%{f}" in
              %record_insert% f acc (%assume% contract label val)
            else
              acc)
          {}
          (%fields% value)
        in
        tail_contract fields_with_contracts label tail_fields
      else
        %blame% (%label_with_message% "missing field %{%head% missing_fields}" label)
    else
      %blame% (%label_with_message% "not a record" label),

  "$dyn_record" = fun contr label value =>
      if %typeof% value == `Record then
          %dictionary_assume% (%go_dict% label) value contr
      else
          %blame% (%label_with_message% "not a record" label),

  "$forall_tail" = fun sealing-key polarity acc label value =>
      if polarity == (%polarity% label) then
          if value == {} then
            let tagged_label = %label_with_message% "polymorphic tail mismatch" label in
            let tail = %record_unseal_tail% sealing-key tagged_label value in
            acc & tail
          else
            %blame% (%label_with_message% "extra field `%{%head% (%fields% value)}`" label)
      else
          # Note: in order to correctly attribute blame, the polarity of `l`
          # must match the polarity of the `forall` which introduced the
          # polymorphic contract (i.e. `pol`). Since we know in this branch
          # that `pol` and `%polarity% l` differ, we swap `l`'s polarity before
          # we continue.
          %record_seal_tail% sealing-key (%chng_pol% label) acc value,

  "$dyn_tail" = fun acc label value => acc & value,

  "$empty_tail" = fun acc label value =>
      if value == {} then acc
      else %blame% (%label_with_message% "extra field `%{%head% (%fields% value)}`" label),

  # Recursive priorities operators

  "$rec_force" = fun value => %rec_force% (%force% value),
  "$rec_default" = fun value => %rec_default% (%force% value),
}
