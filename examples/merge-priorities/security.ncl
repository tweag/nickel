# test = 'ignore'
{
  server.host.options | priority 10 = "TLS",

  # force make it impossible to override this value with false
  firewall = {
    enabled | force = true,
    type | default = "iptables",
    open_ports | priority 5 = [21, 80, 443],
  },
}
