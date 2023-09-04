### https://nixos.org/channels/nixos-unstable nixos
{ pkgs, lib, ... }:
let
  runner-name = "ec2-spot";
in
{
  imports = [ <nixpkgs/nixos/modules/virtualisation/amazon-image.nix> ];
  ec2.hvm = true;

  services.github-runner = {
    enable = true;
    replace = true;
    name = runner-name;
    package = pkgs.github-runner.override {
      # nodejs v16 was marked as EOL ahead of its EOL in https://github.com/NixOS/nixpkgs/pull/229910
      # It will reach EOL only on 2023-09-11
      nodejs_16 = pkgs.nodejs_16.overrideAttrs (orig: {
        meta =
          orig.meta
          // {
            knownVulnerabilities =
              lib.filter
                (x: x != "This NodeJS release has reached its end of life. See https://nodejs.org/en/about/releases/.")
                orig.meta.knownVulnerabilities;
          };
      });
    };
    extraPackages = with pkgs; [
      gawk
      nix
    ];
    url = "${url}";
    tokenFile = "/run/secrets/github-runner.token";
    extraLabels = [
      "EC2"
    ];
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
