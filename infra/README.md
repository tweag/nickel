# GitHub Runner Infrastructure

To redeploy and update GitHub variables, make sure you are logged into AWS and
GitHub using the `awscli2` and `gh` tools:

```console
> nix run nixpkgs#awscli2 -- sts get-caller-identity
{
  # CENSORED
}
❯ nix run github:nixos/nixpkgs#gh -- auth status
github.com
  # CENSORED
  ✓ Token scopes: gist, read:org, repo
```

To log in with AWS SSO credentials, follow [their guide][aws-sso-guide]. To
log into GitHub, you can use `nix run nixpkgs#gh -- auth login` and follow the
instructions.

Note that for updating the GitHub variables, you need the requisite permissions
on the Nickel repository. Then run

```console
nix develop ..#infra -c update-infra
```

## Architecture

The code in this subdirectory provisions AWS infrastucture for starting an
ARM64 GitHub Actions runner on demand. The workflow for producing ARM64 release
artifacts is as follows:

- the release workflow is triggered automatically when a release is created or
  manually for testing
- the workflow requests a runner registration token `$TOKEN` from the GitHub
  API. For this, it needs a personal access token with `repo` scope for the Nickel
  repository.
- the workflow invokes the `$EC2_START` AWS Lambda and provides `$TOKEN` as
  input
- the AWS Lambda stores `$TOKEN` as a parameter in the AWS SSM and requests an
  appropriate EC2 spot instance
- the spot instance boots up, retrieves `$TOKEN` from AWS SSM and starts a
  GitHub Actions runner
- GitHub Actions schedules the ARM64 jobs on the spot instance
- when the jobs building the release artifact have finished, the workflow
  invokes the `$EC2_STOP` AWS Lambda which terminates the EC2 instance

[aws-sso-guide]:  https://docs.aws.amazon.com/cli/latest/userguide/sso-configure-profile-token.html
