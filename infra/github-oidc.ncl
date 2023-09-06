{
  naming_prefix | String,
  github = {
    owner | String,
    repo | String,
    ec2_role = "${resource.aws_iam_role.invoke_lambda_role.arn}",
  },
  lambda.invoke_policy | String,
  config = {
    resource.aws_iam_openid_connect_provider.github_oidc = {
      url = "https://token.actions.githubusercontent.com",
      client_id_list = [
        "sts.amazonaws.com",
      ],
      thumbprint_list
        | doc m%"
          Thumbprints are provided by GitHub, see
          [https://github.blog/changelog/2023-06-27-github-actions-update-on-oidc-integration-with-aws/]
          This should be kept sorted to prevent apparent Terraform drift
        "%
        = [
          "1c58a3a8518e8759bf075b76b750d4f2df264fcd",
          "6938fd4d98bab03faadb97b34396831e3780aea1",
        ],
    },

    resource.aws_iam_role.invoke_lambda_role = {
      name = "%{naming_prefix}-invoke-lambda-role",
      managed_policy_arns = [lambda.invoke_policy],
      assume_role_policy =
        std.serialize
          'Json
          {
            Version = "2012-10-17",
            Statement = [
              {
                Principal.Federated = "${resource.aws_iam_openid_connect_provider.github_oidc.id}",
                Action = "sts:AssumeRoleWithWebIdentity",
                Condition = {
                  StringLike."token.actions.githubusercontent.com:sub" = "repo:%{github.owner}/%{github.repo}:ref:refs/tags/*",
                  StringEquals."token.actions.githubusercontent.com:aud" = "sts.amazonaws.com",
                },
                Effect = "Allow",
              }
            ],
          },
    },
  }
}
