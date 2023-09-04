{
  naming_prefix | String,
  nixos-ami | String,
  vpc.subnet_id,
  region,
  account-id,
  runner = {
    role = "${resource.aws_iam_role.runner.name}",
    role_arn = "${resource.aws_iam_role.runner.arn}",
    instance_type = "c6g.8xlarge",
    nix_config | String,
    launch_template = "${resource.aws_launch_template.nixos-runner.id}",
    launch_template_arn = "${resource.aws_launch_template.nixos-runner.arn}",
  },
  repo_url | String,
  ssm = {
    parameter-path = "/%{naming_prefix}/runner-registration-token",
    parameter-arn = "arn:aws:ssm:%{region}:%{account-id}:parameter%{parameter-path}"
  },
  config = {
    resource.aws_iam_role.runner = {
      name = "%{naming_prefix}-runner-role",
      assume_role_policy =
        std.serialize
          'Json
          {
            Version = "2012-10-17",
            Statement = [
              {
                Action = "sts:AssumeRole",
                Principal.Service = "ec2.amazonaws.com",
                Effect = "Allow",
              }
            ]
          },
    },

    resource.aws_iam_instance_profile.runner = {
      name = "%{naming_prefix}-runner-profile",
      role = runner.role,
    },

    resource.aws_iam_role_policy.runner_ssm_parameters = {
      name = "%{naming_prefix}-ssm_paramters-policy",
      role = runner.role,
      policy =
        std.serialize
          'Json
          {
            Version = "2012-10-17",
            Statement = [
              {
                Action = [
                  "ssm:GetParameter",
                  "ssm:DeleteParameter",
                ],
                Resource = ssm.parameter-arn,
                Effect = "Allow",
              }
            ],
          }
    },

    resource.aws_security_group.allow_egress = {
      name = "%{naming_prefix}-allow-egress",
      vpc_id = "${resource.aws_vpc.runner_vpc.id}",
    },

    resource.aws_security_group_rule.allow_egress = {
      type = "egress",
      from_port = 0,
      to_port = 0,
      protocol = "-1",
      cidr_blocks = ["0.0.0.0/0"],
      ipv6_cidr_blocks = ["::/0"],
      security_group_id = "${resource.aws_security_group.allow_egress.id}",
    },

    resource.aws_launch_template.nixos-runner = {
      name = "%{naming_prefix}-nixos-runner",
      block_device_mappings = [
        {
          device_name = "/dev/xvda",
          ebs = {
            delete_on_termination = true,
            volume_size = 100,
          },
        }
      ],
      image_id = nixos-ami,
      instance_initiated_shutdown_behavior = "terminate",
      instance_type = runner.instance_type,
      network_interfaces = [
        {
          subnet_id = vpc.subnet_id,
          security_groups = ["${resource.aws_security_group.allow_egress.id}"],
          associate_public_ip_address = true,
        }
      ],
      iam_instance_profile.arn = "${resource.aws_iam_instance_profile.runner.arn}",
      user_data = m%"${base64encode(templatefile(%{"\""}%{runner.nix_config}", { url = %{"\""}%{repo_url}", token = %{"\""}%{ssm.parameter-path}" }))}"%,
    },
  }
}
