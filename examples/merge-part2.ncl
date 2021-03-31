{
  server.host.options = "TLS";
  
  firewall.enabled | default = true;
  firewall.type = "iptables";
  firewall.openPorts = [21,80,443];
}
