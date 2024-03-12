# test = 'pass'
{
  user =
    ["jane", "pete", "richie"]
    |> std.array.map
      (
        fun name =>
          {
            username = name,
            email = "%{name}@nickel-lang.org",
          }
      )
}
