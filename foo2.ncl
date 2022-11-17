({
    foo = "Overridden",
} & {
  config.output = {},
}) & {
  foo | default = "blub",

  config.output.value = foo,
}
