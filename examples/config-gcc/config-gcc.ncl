// Validate and normalize gcc flags. They can be either a string `-Wextra` or
// a structured value `{flag = "W", arg = "extra"}`. Arguments are not checked.
let GccFlag =
  // We only allow the following flags
  let available = ["W", "c", "S", "e", "o"] in
  fun label value =>
  if builtins.is_str value then
    if strings.length value > 0 &&
      lists.any (fun x => x == strings.substring 0 1 value) available then
      value
    else
      contracts.blame_with "unknown flag %{value}" label
  else if builtins.is_record value then
    if records.has_field "flag" value && records.has_field "arg" value then
      if lists.any (fun x => x == value.flag) available then
        //Normalize the tag to a string
        value.flag ++ value.arg
      else
        contracts.blame_with "unknown flag %{value.flag}" label
    else
      contracts.blame_with
        "bad record structure: missing field `flag` or `arg`"
        label
  else
    contracts.blame_with "expected record or string" label in

let Path =
  let pattern = m%"^(.+)/([^/]+)$"%m in
  fun label value =>
    if builtins.is_str value then
      if strings.is_match pattern value then
        value
      else
        contracts.blame_with "invalid path" label
    else
      contracts.blame_with "not a string" label in

let SharedObjectFile = fun label value =>
  if builtins.is_str value then
    if strings.is_match m%"\.so$"%m value then
      value
    else
      contracts.blame_with "not an .so file" label
  else
    contracts.blame_with "not a string" label in

let OptLevel = fun label value =>
  if value == 0 || value == 1 || value == 2 then
    value
  else
    contracts.blame label in

let Contract = {
  path_libc | doc "Path to libc."
            | Path
            | SharedObjectFile
            | default = "/lib/x86_64-linux-gnu/libc.so",

  flags | doc "
            Additional flags to pass to GCC. Either provide a string without the
            leading `-`, or a structured value `{flag : Str, arg: Str}`.
          "
        | List GccFlag
        | default = [],

  optimization_level | doc "
                       Optimization level. Possible values:

                        - *0*: unoptimized
                        - *1*: normal
                        - *2*: use optimizations
                      "
                     | OptLevel
                     | default = 1,
} in

{
  flags = ["Wextra", {flag = "o", arg = "stuff.o"}],
  optimization_level = 2,
} | Contract
