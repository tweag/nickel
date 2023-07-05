let users =
  ["jane", "pete", "richie"]
  |> std.array.map
    (
      fun name =>
        {
          username = name,
          email = "%{name}@nickel-lang.org"
        }
    )
in

{
  usernames = users
}
