# test.type = 'pass'


[
  # regression test for issue #908 (https://github.com/tweag/nickel/issues/908)
  let override = { foo = "overridden" } in
  let schema = {
    config | {
      output | {
          value | String,
      }
    },
  } in
  let data = {
    foo | String
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
          value | String,
      }
    },
  } in
  let data = {
    foo | String
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
          value | String,
          ..
      }
    },
  } in
  let fst_data = {
    foo | String
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
      snd_data | String
               | default = "",
      fst_data = "fst",
    },
    snd_data = {
      common.snd = fst_data ++ "_data",
      fst_data | String
               | default = "",
      snd_data = "snd",
    },
    # Recursive priorities are currently on hold since the implementation of
    # RFC005, which changes their semantics. In the meantime, this test has been
    # rewritten temporarily with the expected result of the original "rec force"
    # instead
    #
    # final_override | rec force = {
    #   fst_data = "override",
    #   snd_data = "override",
    #   common.final = fst_data ++ "_" ++ snd_data,
    # },
    final_override = {
      fst_data | force = "override",
      snd_data | force = "override",
      common.final | force = fst_data ++ "_" ++ snd_data,
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
  ] |> std.test.assert_all,
]
|> std.test.assert_all
