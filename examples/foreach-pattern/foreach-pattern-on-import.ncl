# test = 'ignore'
{
  posix_users =
    (import "data_users.yml").users
    |> std.array.map
      (
        fun name =>
          {
            username = name,
            email = "%{name}@nickel-lang.org"
          }
      )
}
