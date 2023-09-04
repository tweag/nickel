{
  region | String,
  github
    | {
      owner | String,
      repo | String,
      ec2_role | String,
      ec2_start | String,
      ec2_stop | String,
      ..
    },
  config = {
    resource.github_actions_variable."ec2_region" = {
      repository = "%{github.repo}",
      variable_name = "EC2_REGION",
      value = region,
    },

    resource.github_actions_secret."ec2_role" = {
      repository = "%{github.repo}",
      secret_name = "EC2_ROLE",
      plaintext_value = github.ec2_role,
    },

    resource.github_actions_secret."ec2_start" = {
      repository = "%{github.repo}",
      secret_name = "EC2_START",
      plaintext_value = github.ec2_start,
    },

    resource.github_actions_secret."ec2_stop" = {
      repository = "%{github.repo}",
      secret_name = "EC2_STOP",
      plaintext_value = github.ec2_stop,
    },
  }
}
