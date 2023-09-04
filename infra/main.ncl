{
  naming_prefix = "tweag-nickel-release-infra",
  github = {
    owner = "tweag",
    repo = "nickel",
  },
  repo_url = "https://github.com/%{github.owner}/%{github.repo}",
  runner.nix_config = "github-runner.nix",
  region,
  account-id = "${data.aws_caller_identity.current.account_id}",
  config = {
    terraform = {
      required_providers = {
        archive.source = "registry.terraform.io/hashicorp/archive",
        aws.source = "registry.terraform.io/hashicorp/aws",
        github.source = "registry.terraform.io/integrations/github",
      },
    },
    provider.aws =
      let region' = region in
      {
        region = region',
        default_tags.tags = {
          owner = naming_prefix,
        }
      },

    provider.github = {
      owner = github.owner,
    },

    data.aws_caller_identity.current = {},
  }
}
& (import "state.ncl")
& (import "lambdas.ncl")
& (import "vpc.ncl")
& (import "runner.ncl")
& (import "github-oidc.ncl")
& (import "github-variables.ncl")
