let {check, ..} = import "lib/assert.ncl" in

[
  # regression test for issue #908 (https://github.com/tweag/nickel/issues/908)
  let override = { foo = "overridden" } in
  let schema = {
    config | {
      output | {
          value | Str,
      }
    },
  } in
  let data = {
    foo | Str
      | default = "original",

    config.output.value = foo,
  } in
  (override & (schema & data))
  == {

    foo = "overridden",
    config.output.value = "overridden",
  },

  # 2-stage override
  let fst_override = { foo = "first override"} in
  let snd_override = { foo | force = "second override"} in
  let schema = {
    config | {
      output | {
          value | Str,
      }
    },
  } in
  let data = {
    foo | Str
        | default = "original",

    config.output.value = foo,
  } in
  (fst_override & (schema & data) & snd_override)
  == {
    foo = "second override",
    config.output.value = "second override",
  },

  # 2-stage overriding, with a 3-way merging
  let fst_override = { foo = "first override"} in
  let snd_override = { foo | force = "second override"} in
  let schema = {
    config | {
      output | {
          value | Str,
          ..
      }
    },
  } in
  let fst_data = {
    foo | Str
        | default = "original",

    config.output.value = foo,
  } in
  let snd_data = {
    foo,
    config.output.snd_data = foo,
  } in
  (fst_override
  & (schema & fst_data & snd_data)
  & snd_override)
  == {
    foo = "second override",
    config.output.value = "second override",
    config.output.snd_data = "second override",
  },

  # merging recursive expressions with different dependencies
  let parent = {
    fst_data = {
      common.fst = snd_data ++ "_data",
      snd_data | Str
               | default = "",
      fst_data = "fst",
    },
    snd_data = {
      common.snd = fst_data ++ "_data",
      fst_data | Str
               | default = "",
      snd_data = "snd",
    },
    final_override | rec force = {
      fst_data = "override",
      snd_data = "override",
      common.final = fst_data ++ "_" ++ snd_data,
    },
  } in
  [
    parent.fst_data & parent.snd_data == {
      common.fst = "snd_data",
      common.snd = "fst_data",
      fst_data = "fst",
      snd_data = "snd",
    },
    (parent.fst_data & parent.snd_data) & parent.final_override == {
      common.fst = "override_data",
      common.snd = "override_data",
      common.final = "override_override",
      fst_data = "override",
      snd_data = "override",
    }
  ] |> check,
]
|> check
