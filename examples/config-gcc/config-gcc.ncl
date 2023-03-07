# test: pass

# Validate and normalize gcc flags. They can be either a string `-Wextra` or
# a structured value `{flag = "W", arg = "extra"}`. Arguments are not checked.
let GccFlag =
  # We only allow the following flags
  let available = ["W", "c", "S", "e", "o"] in
  fun label value =>
  if builtin.is_string value then
    if string.length value > 0 &&
      array.any (fun x => x == string.substring 0 1 value) available then
      value
    else
      contract.blame_with_message "unknown flag %{value}" label
  else if builtin.is_record value then
    if record.has_field "flag" value && record.has_field "arg" value then
      if array.any (fun x => x == value.flag) available then
        #Normalize the tag to a string
        value.flag ++ value.arg
      else
        contract.blame_with_message "unknown flag %{value.flag}" label
    else
      contract.blame_with_message
        "bad record structure: missing field `flag` or `arg`"
        label
  else
    contract.blame_with_message "expected record or string" label in

let Path =
  let pattern = m%"^(.+)/([^/]+)$"% in
  fun label value =>
    if builtin.is_string value then
      if string.is_match pattern value then
        value
      else
        contract.blame_with_message "invalid path" label
    else
      contract.blame_with_message "not a string" label in

let SharedObjectFile = fun label value =>
  if builtin.is_string value then
    if string.is_match m%"\.so$"% value then
      value
    else
      contract.blame_with_message "not an .so file" label
  else
    contract.blame_with_message "not a string" label in

let OptLevel = fun label value =>
  if value == 0 || value == 1 || value == 2 then
    value
  else
    contract.blame label in

let Contract = {
  path_libc | doc "Path to libc."
            | Path
            | SharedObjectFile
            | default = "/lib/x86_64-linux-gnu/libc.so",

  flags | doc "
            Additional flags to pass to GCC. Either provide a string without the
            leading `-`, or a structured value `{flag : String, arg: String}`.
          "
        | Array GccFlag
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
