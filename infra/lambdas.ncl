{
  naming_prefix | String,
  region | String,
  account-id | String,
  vpc.subnet_id | String,
  runner
    | {
      launch_template | String,
      launch_template_arn | String,
      role_arn | String,
      instance_type | String,
      instance_tag = naming_prefix,
      ..
    },
  lambda.invoke_policy = "${resource.aws_iam_policy.lambda_invoke.arn}",
  ssm
    | {
      parameter-path | String,
      parameter-arn | String,
    },
  github = {
    ec2_start = "${resource.aws_lambda_function.spot_start.function_name}",
    ec2_stop = "${resource.aws_lambda_function.spot_stop.function_name}",
  },
  config = {
    data.archive_file.lambda = {
      type = "zip",
      source_dir = "${path.module}/spot_lambdas/",
      output_path = "${path.module}/build/lambda.zip",
    },

    resource.aws_lambda_function.spot_start = {
      function_name = "%{naming_prefix}-start",
      filename = config.data.archive_file.lambda.output_path,
      source_code_hash = "${data.archive_file.lambda.output_base64sha256}",
      role = "${resource.aws_iam_role.lambda_execution_role.arn}",
      runtime = "python3.11",
      handler = "start.lambda_handler",
      timeout = 120,

      environment.variables = {
        LAUNCH_TEMPLATE = runner.launch_template,
        SSM_PARAMETER = ssm.parameter-path,
        TAG_KEY = runner.instance_tag,
      }
    },

    resource.aws_lambda_function.spot_stop = {
      function_name = "%{naming_prefix}-stop",
      filename = config.data.archive_file.lambda.output_path,
      source_code_hash = "${data.archive_file.lambda.output_base64sha256}",
      role = "${resource.aws_iam_role.lambda_execution_role.arn}",
      runtime = "python3.11",
      handler = "stop.lambda_handler",
      timeout = 120,
      environment.variables = {
        TAG_KEY = runner.instance_tag,
      }
    },

    resource.aws_iam_role.lambda_execution_role = {
      name = "%{naming_prefix}-lambda-execution-role",
      assume_role_policy =
        std.serialize
          'Json
          {
            Version = "2012-10-17",
            Statement = [
              {
                Action = "sts:AssumeRole",
                Principal.Service = "lambda.amazonaws.com",
                Effect = "Allow",
              }
            ]
          },
    },

    resource.aws_iam_role_policy.lambda_ssm_parameters = {
      name = "%{naming_prefix}-lambda-ssm-parameter-policy",
      role = "${resource.aws_iam_role.lambda_execution_role.name}",
      policy =
        std.serialize
          'Json
          {
            Version = "2012-10-17",
            Statement = [
              {
                Action = [
                  "ssm:PutParameter"
                ],
                Resource = ssm.parameter-arn,
                Effect = "Allow",
              }
            ],
          }
    },

    resource.aws_iam_role_policy.lambda_ec2_policy = {
      name = "%{naming_prefix}-lambda-ec2-policy",
      role = "${resource.aws_iam_role.lambda_execution_role.name}",
      policy =
        std.serialize
          'Json
          {
            Version = "2012-10-17",
            Statement = [
              {
                Action = [
                  "ec2:DescribeInstances",
                ],
                Resource = "*",
                Effect = "Allow",
              },
              {
                Action = [
                  "ec2:RunInstances"
                ],
                Condition = {
                  StringEquals."ec2:LaunchTemplate" = runner.launch_template_arn,
                  StringEqualsIfExists."ec2:InstanceType" = runner.instance_type,
                  "Bool"."ec2:IsLaunchTemplateResource" = "true",
                },
                Resource = "*",
                Effect = "Allow",
              },
              {
                Action = [
                  "ec2:RunInstances"
                ],
                Condition = {
                  "ForAllValues:StringNotEquals"."aws:TagKeys" = runner.instance_tag,
                },
                Resource = "arn:aws:ec2:*:*:instance/*",
                Effect = "Deny",
              },
              {
                Action = [
                  "ec2:TerminateInstances"
                ],
                Condition = {
                  StringLike."aws:ResourceTag/%{runner.instance_tag}" = "*"
                },
                Resource = ["arn:aws:ec2:%{region}:%{account-id}:instance/*"],
                Effect = "Allow",
              },
              {
                Action = [
                  "iam:PassRole"
                ],
                Resource = [runner.role_arn],
                Effect = "Allow",
              },
              {
                Action = [
                  "ec2:CreateTags"
                ],
                Condition = {
                  StringEquals."ec2:CreateAction" = "RunInstances",
                  StringLike."aws:RequestTag/%{runner.instance_tag}" = "*",
                },
                Resource = ["arn:*:ec2:%{region}:%{account-id}:*/*"],
                Effect = "Allow",
              }
            ],
          }
    },

    resource.aws_iam_policy.lambda_invoke = {
      name = "%{naming_prefix}-lambda-invoke-policy",
      policy =
        std.serialize
          'Json
          {
            Version = "2012-10-17",
            Statement = [
              {
                Action = "lambda:InvokeFunction",
                Resource = [
                  "${resource.aws_lambda_function.spot_start.arn}",
                  "${resource.aws_lambda_function.spot_stop.arn}",
                ],
                Effect = "Allow",
              }
            ],
          }
    },
  }
}
