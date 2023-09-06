# GitHub Runner Infrastructure

If you make any changes to the infrastructure code in this directory, you will
have to redeploy it. Do the following:

1. Make sure you're logged into AWS. You can check using `awscli2`:

   ```console
   ❯ nix run nixpkgs#awscli2 -- sts get-caller-identity
   {
     # CENSORED
   }
   ```

   If this fails, log in with AWS SSO credentials, following [their guide][aws-sso-guide].
   You will likely hit [a bug in the AWS provider][aws-provider-bug]. Apply the
   [workaround mentioned in a comment][aws-provider-workaround] and log in again
   using `aws sso login` to resolve this.

2. Make sure you're logged into GitHub. You can check using `gh`:

   ```console
   ❯ nix run github:nixos/nixpkgs#gh -- auth status
   github.com
     # CENSORED
     ✓ Token scopes: gist, read:org, repo
   ```

   If this fails, log in using `nix run nixpkgs#gh -- auth login` and follow
   the instructions.

3. Update the infrastructure using

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

[aws-sso-guide]: https://docs.aws.amazon.com/cli/latest/userguide/sso-configure-profile-token.html
[aws-provider-bug]: https://github.com/aws/aws-cli/issues/7632
[aws-provider-workaround]: https://github.com/aws/aws-cli/issues/7632#issuecomment-1568458315
