{
  naming_prefix | String,
  region | String,
  state = {
    bucket | String = "%{naming_prefix}-tfstate",
    key | String = "%{naming_prefix}.tfstate",
    dynamodb | String = "%{naming_prefix}-tfstate-lock",
  },
  config = {
    terraform.backend.s3 =
      let region' = region in
      {
        bucket = state.bucket,
        key = state.key,
        region = region',
        dynamodb_table = state.dynamodb,
      },

    resource.aws_s3_bucket.tfstate = {
      bucket = state.bucket,
    },

    resource.aws_s3_bucket_versioning.tfstate-versioning = {
      bucket = "${aws_s3_bucket.tfstate.id}",
      versioning_configuration.status = "Enabled",
    },

    resource.aws_dynamodb_table.tfstate-lock = {
      name = state.dynamodb,
      read_capacity = 5,
      write_capacity = 5,
      hash_key = "LockID",
      attribute = [
        {
          name = "LockID",
          type = "S"
        }
      ]
    },
  }
}
