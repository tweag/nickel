// Validate and normalize gcc flags. They can be either a string `-Wextra` or
// a structured value `{flag = "W", arg = "extra"}`. Arguments are not checked.
let GccFlag =
  // We only allow the following flags
  let available = ["W", "c", "S", "e", "o"] in
  fun label value =>
  if builtins.isStr value then
    if strings.length value > 0 &&
      lists.any (fun x => x == strings.substring 0 1 value) available then
      value
    else
      contracts.blameWith "unknown flag #{value}" label
  else if builtins.isRecord value then
    if records.hasField "flag" value && records.hasField "arg" value then
      if lists.any (fun x => x == value.flag) available then
        //Normalize the tag to a string
        value.flag ++ value.arg
      else
        contracts.blameWith "unknown flag #{value.flag}" label
    else
      contracts.blameWith
        "bad record structure: missing field `flag` or `arg`"
        label
  else
    contracts.blameWith "expected record or string" label in

let Path =
  let pattern = m#"^(.+)/([^/]+)$"#m in
  fun label value =>
    if builtins.isStr value then
      if strings.isMatch pattern value then
        value
      else
        contracts.blameWith "invalid path" label
    else
      contracts.blameWith "not a string" label in

let SharedObjectFile = fun label value =>
  if builtins.isStr value then
    if strings.isMatch m#"\.so$"#m value then
      value
    else
      contracts.blameWith "not an .so file" label
  else
    contracts.blameWith "not a string" label in

let OptLevel = fun label value =>
  if value == 0 || value == 1 || value == 2 then
    value
  else
    contracts.blame label in

let Contract = {
  pathLibC | doc "Path to libc."
           | #Path
           | #SharedObjectFile
           | default = "/lib/x86_64-linux-gnu/libc.so",

  flags | doc "
            Additional flags to pass to GCC. Either provide a string without the
            leading `-`, or a structured value `{flag : Str, arg: Str}`.
          "
        | List #GccFlag
        | default = [],

  optimizationLevel | doc "
                       Optimization level. Possible values:

                        - *0*: unoptimized
                        - *1*: normal
                        - *2*: use optimizations
                      "
                    | #OptLevel
                    | default = 1,
} in

{
  flags = ["Wextra", {flag = "o", arg = "stuff.o"}],
  optimizationLevel = 2,
} | #Contract
