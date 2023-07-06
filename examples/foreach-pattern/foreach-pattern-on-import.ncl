# test = 'ignore'
# fails because YAML files are source filtered
let users =
  (import "data_users.yml").users
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
  posix_users = users
}
