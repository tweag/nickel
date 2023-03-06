{
  contract = {
    blame
      | doc m%"
        Raise blame for a given label.

        Type: `forall a. Lbl -> a`
        (for technical reasons, this function isn't actually statically typed)

        Blame is the mechanism to signal contract violiation in Nickel. It ends
        the program execution and print a detailed report thanks to the
        information tracked inside the label.

        For example:

        ```nickel
        IsZero = fun label value =>
          if value == 0 then
            value
          else
            contract.blame label
        ```
        "%
      = fun label => %blame% label,

    blame_with_message
      | doc m%"
        Raise blame with respect to a given label and a custom error message.

        Type: `forall a. Str -> Lbl -> a`
        (for technical reasons, this function isn't actually statically typed)

        Same as `blame`, but take an additional custom error message that will be
        displayed as part of the blame error. `blame_with_message message label`
        is equivalent to `blame (label.with_message message label)`

        For example:

        ```nickel
        let IsZero = fun label value =>
          if value == 0 then
            value
          else
            contract.blame_with_message_message "Not zero" label
        in

        0 | IsZero
        ```
        "%
      = fun message label => %blame% (%label_with_message% message label),

    from_predicate
      | doc m%"
        Generate a contract from a boolean predicate.

        Type: `(Dyn -> Bool) -> (Lbl -> Dyn -> Dyn)`
        (for technical reasons, this function isn't actually statically typed)

        For example:

        ```nickel
        let IsZero = contract.from_predicate (fun x => x == 0) in
        0 | IsZero
        ```
        "%
      = fun pred label value => if pred value then value else %blame% label,

    label
      | doc m%"
          The label submodule, which gathers functions that manipulate contract
          labels.

          A label is a special opaque value automatically passed by the Nickel
          interpreter to contracts at contract application. A label offers a way
          for custom contracts to customize the error message that will be shown
          if the contract is broken.
        "%
      = {
        with_message
          | doc m%"
            Attach a custom error message to a label.

            Type: `Str -> Lbl -> Lbl`
            (for technical reasons, this function isn't actually statically typed)

            If a custom error message was previously set, there are two
            possibilities:
              - the label has gone through a `contract.apply` call in-between.
                In this case, the previous custom reporting data are stacked,
                and using `with_message` won't erase anything, but just add
                a new diagnostic on the stack.
              - no `contract.apply` has taken place since the last message was
                set. In this case, the current diagnostic is still the same, and
                the previous error message will be erased.

            For example:

            ```nickel
            let ContractNum = contract.from_predicate (fun x => x > 0 && x < 50) in

            let Contract = fun label value =>
              if builtin.is_num value then
                contract.apply
                  ContractNum
                  (contract.label.with_message
                    "num subcontract failed! (out of bound)"
                    label
                  )
                  value
              else
                value
            in

            5 | Contract
            ```
            "%
          = fun message label => %label_with_message% message label,

        with_notes
          | doc m%"
            Attach custom error notes to a label.

            Type: `Array Str -> Lbl -> Lbl`
            (for technical reasons, this function isn't actually statically typed)

            If custom error notes were previously set, there are two
            possibilities:
              - the label has gone through a `contract.apply` call in-between.
                In this case, the previous reporting data are stacked,
                and using `with_notes` won't erase anything, but just add
                a new diagnostic on the stack.
              - no `contract.apply` has taken place since the last notes were
                set. In this case, the current diagnostic is still the same, and
                the previous error notes will be erased.

            For example:

            ```nickel
            let ContractNum = contract.from_predicate (fun x => x > 0 && x < 50) in

            let Contract = fun label value =>
              if builtin.is_num value then
                contract.apply
                  ContractNum
                  (label
                   |> contract.label.with_message "num subcontract failed! (out of bound)"
                   |> contract.label.with_notes [
                        "The value was a number, but this number is out of the expected bound",
                        "The value must be a number between 0 and 50, both excluded",
                      ]
                  )
                  value
              else
                value
            in

            5 | Contract
            ```
            "%
          # the %label_with_notes% operator expect an array of string that is
          # fully evaluated, thus we force the notes first
          = fun notes label => %label_with_notes% (%force% notes) label,

        append_note
          | doc m%"
              Append a note to the label's error notes.

              Type: `Str -> Lbl -> Lbl`
              (for technical reasons, this function isn't actually statically typed)
            "%
          = fun note label => %label_append_note% note label,
    },

    apply
      | doc m%"
          Apply a contract to a label and a value.

          Type: `Contract -> Lbl -> Dyn -> Dyn`
          (for technical reasons, this function isn't actually statically typed)

          Nickel supports user-defined contracts defined as functions, but also
          as records. Moreover, the interpreter performs additional book-keeping
          for error reporting when applying a contract in an expression
          `value | Contract`. You should not use standard function application
          to apply a contract, but this function instead.

          # Example

          ```nickel
          let Nullable = fun param_contract label value =>
            if value == null then null
            else contract.apply param_contract label value
          in
          let Contract = Nullable {foo | Num} in
          ({foo = 1} | Contract)
          ```

          # Diagnostic stack

          Using `apply` will stack the current custom reporting data, and create a
          fresh current working diagnostic. `apply` thus acts automatically as a
          split point between a contract and its subcontracts, providing the
          subcontracts a fresh label to add their diagnostic to, while remembering
          the previous reporting data set by the parent contract.

          ## Illustration

          ```nickel
          let ChildContract = fun label value =>
            label
            |> contract.label.with_message "child's message"
            |> contract.label.append_note "child's note"
            |> contract.blame
          in

          let ParentContract = fun label value =>
            let label =
              label
              |> contract.label.with_message "parent's message"
              |> contract.label.append_note "parent's note"
            in
            contract.apply ChildContract label value
          in

          null | ParentContract
          ```

          This example will print two diagnostics: the main one using, the
          message and note of the child contract, and a secondary diagnostic,
          with parent's contract message and note.
        "%
      = fun contract label value =>
        %assume% contract (%label_push_diag% label) value,
  },
}
