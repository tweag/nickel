# test = 'pass'

# Validate and normalize gcc flags. They can be either a string `-Wextra` or
# a structured value `{flag = "W", arg = "extra"}`. Arguments are not checked.
let GccFlag =
  # We only allow the following flags
  let available = ["W", "c", "S", "e", "o"] in
  fun label value =>
    std.typeof value
    |> match {
      'String =>
        if std.string.length value > 0
        && std.array.any ((==) (std.string.substring 0 1 value)) available then
          value
        else
          std.contract.blame_with_message "unknown flag %{value}" label,
      'Record =>
        if std.record.has_field "flag" value && std.record.has_field "arg" value then
          if std.array.any ((==) value.flag) available then
            #Normalize the tag to a string
            value.flag ++ value.arg
          else
            std.contract.blame_with_message "unknown flag %{value.flag}" label
        else
          std.contract.blame_with_message
            "bad record structure: missing field `flag` or `arg`"
            label,
      _ => std.contract.blame_with_message "expected record or string" label,
    }
in

let Path =
  let pattern = m%"^(.+)/([^/]+)$"% in
  fun label value =>
    if std.is_string value then
      if std.string.is_match pattern value then
        value
      else
        std.contract.blame_with_message "invalid path" label
    else
      std.contract.blame_with_message "not a string" label
in

let SharedObjectFile = fun label value =>
  if std.is_string value then
    if std.string.is_match m%"\.so$"% value then
      value
    else
      std.contract.blame_with_message "not an .so file" label
  else
    std.contract.blame_with_message "not a string" label
in

let OptLevel = fun label value =>
  if value == 0 || value == 1 || value == 2 then
    value
  else
    std.contract.blame label
in

let Contract = {
  path_libc
    | doc "Path to libc."
    | Path
    | SharedObjectFile
    | default
    = "/lib/x86_64-linux-gnu/libc.so",

  flags
    | doc m%"
        Additional flags to pass to GCC. Either provide a string without the
        leading `-`, or a structured value `{flag : String, arg: String}`.
      "%
    | Array GccFlag
    | default
    = [],

  optimization_level
    | doc m%"
        Optimization level. Possible values:

         - *0*: unoptimized
         - *1*: normal
         - *2*: use optimizations
      "%
    | OptLevel
    | default
    = 1,
}
in

(
  {
    flags = ["Wextra", { flag = "o", arg = "stuff.o" }],
    optimization_level = 2,
  } | Contract
)
