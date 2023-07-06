# test = 'ignore'

# Nickel can import plain yaml, or json files
let _users = (import "data_users.yml") in
let _groups = (import "data_groups.json") in

# It even imports toml
let _machines = (import "data_machines.toml") in

# And of course other nickel files
let _nickel_properties = (import "data_nickel_properties.ncl") in

# This is the output object
{
  users = _users.users,
  groups = _groups.groups,
  machines = _machines.machines,
  off_topic = {
    nickel_properties = _nickel_properties
  }
}
