### https://nixos.org/channels/nixos-23.11 nixos
{ pkgs, lib, ... }:
let
  runner-name = "ec2-spot";
in
{
  imports = [ <nixpkgs/nixos/modules/virtualisation/amazon-image.nix> ];
  ec2.hvm = true;

  services.github-runners."$${runner-name}" = {
    enable = true;
    replace = true;
    nodeRuntimes = [ "node20" ];
    extraPackages = with pkgs; [
      gh
      docker
      gawk
      nix
    ];
    url = "${url}";
    tokenFile = "/run/secrets/github-runner.token";
    extraLabels = [
      "EC2"
    ];
    serviceOverrides = {
      Group = "docker";
    };
  };

  virtualisation.docker.enable = true;

  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
    substituters = [ "https://tweag-nickel.cachix.org" ];
    trusted-public-keys = [ "tweag-nickel.cachix.org-1:GIthuiK4LRgnW64ALYEoioVUQBWs0jexyoYVeLDBwRA=" ];
    accept-flake-config = true;
  };

  systemd.services.github-runner-init = {
    description = "Setup tasks before trying to start the GitHub runner";

    before = [ "github-runner-$${runner-name}.service" ];
    requiredBy = [ "github-runner-$${runner-name}.service" ];

    script = ''
      #!$${pkgs.runtimeShell} -eu
      umask 077
      mkdir -p /run/secrets
      while ! $${pkgs.awscli2}/bin/aws sts get-caller-identity; do
        sleep 10
      done
      $${pkgs.awscli2}/bin/aws ssm get-parameter --name '${token}' --with-decryption --output json --query 'Parameter.Value' \
        | $${pkgs.jq}/bin/jq -rj > /run/secrets/github-runner.token
      $${pkgs.awscli2}/bin/aws ssm delete-parameter --name '${token}'
    '';

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
  };
}
