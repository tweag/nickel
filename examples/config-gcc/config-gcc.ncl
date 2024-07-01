# test = 'pass'

# Validate and normalize gcc flags. They can be either a string `-Wextra` or
# a structured value `{flag = "W", arg = "extra"}`. Arguments are not checked.
let GccFlag =
  let supported_flags = ["W", "c", "S", "e", "o"] in
  let is_valid_flag
    | doc "check if a string of length > 0 is a valid flag"
    = fun string =>
      std.array.elem (std.string.substring 0 1 string) supported_flags
    in

  std.contract.custom
    (
      match {
        value if std.is_string value && is_valid_flag value => 'Done,
        { flag, arg } if std.array.elem flag supported_flags => 'Ok,
        value if std.is_string value =>
          'Error { message = "unknown flag %{value}" },
        { flag, arg = _ } =>
          'Error { message = "unknown flag %{flag}" },
        { .. } =>
          'Error {
            message = "bad record structure: missing field `flag` or `arg`",
          },
        _ => 'Error { message = "expected record or string" },
      }
    )
    # The delayed part is only used for normalization. Given the immediate part,
    # we enter the delayed part if and only if the flag has the form
    # `{flag, arg}`. We don't perform any check and just normalize it to a
    # string
    (fun _ value => "%{value.flag}%{value.arg}")
in

let Path =
  let pattern = m%"^(.+)/([^/]+)$"% in
  std.contract.from_validator
    (
      fun value =>
        if std.is_string value then
          if std.string.is_match pattern value then
            'Ok
          else
            'Error { message = "invalid path" }
        else
          'Error { message = "not a string" }
    )
in

let SharedObjectFile =
  std.contract.from_validator
    (
      fun value =>
        if std.is_string value then
          if std.string.is_match m%"\.so$"% value then
            'Ok
          else
            'Error { message = "not an .so file" }
        else
          'Error { message = "not a string" }
    )
in

let OptLevel =
  std.contract.from_predicate
    (
      match {
        0 or 1 or 2 => true,
        _ => false,
      }
    )
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
