{
  # Internal operations. Can't be accessed from user code because `$` is not a
  # valid starting character for an identifier.

  # Contract implementations
  "$dyn" = fun l t => t,

  "$num" = fun l t => if %typeof% t == `Num then t else %blame% l,

  "$bool" = fun l t => if %typeof% t == `Bool then t else %blame% l,

  "$string" = fun l t => if %typeof% t == `Str then t else %blame% l,

  "$fail" = fun l t => %blame% l,

  "$array" = fun elt l t =>
    if %typeof% t == `Array then
        %array_lazy_assume% (%go_array% l) t elt
    else
        %blame% l,

  "$func" = fun s t l e =>
      if %typeof% e == `Fun then
          (fun x => %assume% t (%go_codom% l) (e (%assume% s (%chng_pol% (%go_dom% l)) x)))
      else
          %blame% l,

  "$forall_var" = fun sy pol l t =>
      let lPol = %polarity% l in
      if pol == lPol then
          %unseal% sy t (%blame% l)
      else
          # Here, we know that this term should be sealed, but to give the right
          # blame for the contract, we have to change the polarity to match the
          # polarity of the `Forall`, because this is what's important for
          # blaming polymorphic contracts.
          %seal% sy (%chng_pol% l) t,

  "$enums" = fun case l t =>
      if %typeof% t == `Enum then
          %assume% case l t
      else
          %blame% (%label_with_message% "not an enum tag" l),

  "$enum_fail" = fun l =>
      %blame% (%label_with_message% "tag not included in the enum type" l),

  "$record" = fun field_contracts tail_contract l t =>
    if %typeof% t == `Record then
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

      let contracts_not_in_t = field_diff field_contracts t in
      let missing_fields = %fields% contracts_not_in_t in

      if %length% missing_fields == 0 then
        let tail_fields = field_diff t field_contracts in
        let fields_with_contracts = array.fold_left
          (fun acc f =>
            if %has_field% f field_contracts then
              let contract = field_contracts."%{f}" in
              let label = %go_field% f l in
              let val = t."%{f}" in
              %record_insert% f acc (%assume% contract label val)
            else
              acc)
          {}
          (%fields% t)
        in
        tail_contract fields_with_contracts l tail_fields
      else
        %blame% (%label_with_message% "missing field %{%head% missing_fields}" l)
    else
      %blame% (%label_with_message% "not a record" l),

  "$dyn_record" = fun contr l t =>
      if %typeof% t == `Record then
          %dictionary_assume% (%go_dict% l) t contr
      else
          %blame% (%label_with_message% "not a record" l),

  "$forall_tail" = fun sy pol acc l t =>
      if pol == (%polarity% l) then
          if t == {} then
            let tagged_l = %label_with_message% "polymorphic tail mismatch" l in
            let tail = %record_unseal_tail% sy tagged_l t in
            acc & tail
          else
            %blame% (%label_with_message% "extra field `%{%head% (%fields% t)}`" l)
      else
          # Note: in order to correctly attribute blame, the polarity of `l`
          # must match the polarity of the `forall` which introduced the
          # polymorphic contract (i.e. `pol`). Since we know in this branch
          # that `pol` and `%polarity% l` differ, we swap `l`'s polarity before
          # we continue.
          %record_seal_tail% sy (%chng_pol% l) acc t,

  "$dyn_tail" = fun acc l t => acc & t,

  "$empty_tail" = fun acc l t =>
      if t == {} then acc
      else %blame% (%label_with_message% "extra field `%{%head% (%fields% t)}`" l),

  # Recursive priorities operators

  "$rec_force" = fun val => %rec_force% (%force% val),
  "$rec_default" = fun val => %rec_default% (%force% val),
}
