{
  server.host.options = "TLS",
  
  firewall.enabled | default = true,
  firewall.type = "iptables",
  firewall.open_ports = [21, 80, 443],
}
