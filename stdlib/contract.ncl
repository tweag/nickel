{
  contract = {
    blame
      | doc m%"
        Raise blame for a given label.

        Type: `forall a. Lbl -> a`
        (for technical reasons, this element isn't actually statically typed)

        Blame is the mechanism to signal contract violiation in Nickel. It ends
        the program execution and print a detailed report thanks to the
        information tracked inside the label.

        For example:
        ```nickel
        IsZero = fun label value =>
          if value == 0 then value
          else contract.blame label
        ```
        "%
      = fun l => %blame% l,

    blame_with
      | doc m%"
        Raise blame with respect to a given label and a custom error message.

        Type: `forall a. Str -> Lbl -> a`
        (for technical reasons, this element isn't actually statically typed)

        Same as `blame`, but take an additional custom error message that will be
        displayed as part of the blame error. `blame_with msg l` is equivalent to
        `blame (tag msg l)

        For example:
        ```nickel
        let IsZero = fun label value =>
          if value == 0 then value
          else contract.blame_with "Not zero" label in
        0 | IsZero
        ```
        "%
      = fun msg l => %blame% (%tag% msg l),

    from_predicate
      | doc m%"
        Generate a contract from a boolean predicate.

        Type: `(Dyn -> Bool) -> (Lbl -> Dyn -> Dyn)`
        (for technical reasons, this element isn't actually statically typed)

        For example:
        ```
        let IsZero = contract.from_predicate (fun x => x == 0) in
        0 | IsZero
        ```
        "%
      = fun pred l v => if pred v then v else %blame% l,

    tag
      | doc m%"
        Attach a tag, or a custom error message, to a label. If a tag was
        previously set, it is erased.

        Type: `Str -> Lbl -> Lbl`
        (for technical reasons, this element isn't actually statically typed)

        For example:
        ```
        let ContractNum = contract.from_predicate (fun x => x > 0 && x < 50) in
        Contract = fun label value =>
          if builtin.is_num value then
            ContractNum
              (contract.tag "num subcontract failed! (out of bound)" label)
              value
          else
            value in
        5 | Contract
        ```
        "%
      = fun msg l => %tag% msg l,

    apply
      | doc m%"
        Apply a contract to a label and a value.

        Type: `Contract -> Lbl -> Dyn -> Dyn`
        (for technical reasons, this element isn't actually statically typed)

        Nickel supports user-defined contracts defined as functions, but also as
        records. Moreover, the interpreter performs additional book-keeping for
        error reporting when applying a contract in an expression `value |
        Contract`. You should not use standard function application to apply a
        contract, but this function instead.

        For example:
        ```
        let Nullable = fun param_contract label value =>
          if value == null then null
          else contract.apply param_contract label value
        in
        let Contract = Nullable {foo | Num} in
        ({foo = 1} | Contract)
        ```
        "%
      = fun contract label value => %assume% contract label value,
  },
}
