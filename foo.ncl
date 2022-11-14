let correct_ =
{
    foo = "Overridden",
} & {
  config | {
    output | {
        value | Str,
    }
  },
} & {
  foo | Str
    | default = "blub",

  config.output.value = foo,
} in
let wrong_ =
{
    foo = "Overridden",
} & ({
  config | {
    output | {
        value | Str,
    }
  },
} & {
  foo | Str
    | default = "blub",

  config.output.value = foo,
}) in
let wrong2_ =
{
    foo = "Overridden",
} & ({
  config.output = {},
} & {
  foo | default = "blub",

  config.output.value = foo,
}) in

{
  correct = correct_,
  wrong = wrong_,
  wrong2 = wrong2_,
}
