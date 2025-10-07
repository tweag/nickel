let discriminate_over
  | String -> { _ | Dyn } -> Dyn
  | doc m%"
    Take the name of a field, a record contract with this field set to a
    constant value, and wrap it into a new contract that accept roughly the
    same values, but where the check that `field` is set to a specific value is
    done _eagerly_.

    This makes it possible to use `record_contract` in an `any_of` combination
    with other record contracts that have the same structure, which would hit
    the known limitations of `any_of` related to laziness.
  "%
  = fun field RecordContract =>
    std.contract.custom (fun label other =>
      let field_value = RecordContract."%{field}" in

      # We just check eagerly if the field is not there or there with the wrong
      # value, and error eagerly. In all other cases (whether the contract
      # passes or not), we delegate the checks and the error messages to the
      # original record contract implementation
      if std.is_record other
      && (
        !std.record.has_field field other
        || other."%{field}" != RecordContract."%{field}"
      ) then
        'Error {
          message = "unexpected or missing value for %{field}",
        }
      else
        std.contract.check RecordContract label other
    )
in

let any_of_over = fun field contracts =>
  contracts
  |> std.array.map (discriminate_over field)
  |> std.contract.any_of
  in

{
    Output = any_of_over "ty" [OutputLib, OutputBin, OutputData],

    OutputLib = {
        ty = 'OutputLib,
        glob
          | String,
    },
    OutputBin = {
        ty = 'OutputBin,
        glob
          | Number,
    },
    OutputData = {
        ty = 'OutputData,
        glob
          | Bool,
    },
}
