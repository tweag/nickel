# test: pass

# Merge several blocks into one final configuration. In a real world case, one
# would also want contracts to validate the shape of the data.
let server = import "server.ncl" in
let security = import "security.ncl" in
# Disabling firewall in the final result
server & security & {
  # As opposed to the simple merge example, this would now fail
  # firewall.enabled = false

  firewall.open_ports | priority 10 = [80],
  firewall.type = "superiptables",
  server.host.ip = "89.22.11.01",

  # this will only be selected if no values with higher priority (or no priority
  # annotation, which is the same as priority 0) is ever defined
  # because there's a definite value below, this won't be selected.
  server.host.name | priority -1 = "hello-world.backup.com",
} & {
  server.host.name = "hello-world.main.com",
}
