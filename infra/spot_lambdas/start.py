import os
import boto3

ssm = boto3.client('ssm')
ec2 = boto3.client('ec2')

def lambda_handler(event, context):
    tag_key = os.environ.get('TAG_KEY')
    launch_template_id = os.environ.get('LAUNCH_TEMPLATE')
    ssm_parameter = os.environ.get('SSM_PARAMETER')

    ssm.put_parameter(
        Name=ssm_parameter,
        Value=event['runner_token'],
        Type="SecureString",
        Overwrite=True,
    )
    response = ec2.run_instances(
        MinCount=1,
        MaxCount=1,
        LaunchTemplate={
            "LaunchTemplateId": launch_template_id,
            "Version": "$Latest"
        },
        TagSpecifications=[
            {
                "ResourceType": "instance",
                "Tags": [
                    {
                        "Key": tag_key,
                        "Value": event['ref_name']
                    }
                ]
            }
        ]
    )

    instance_id = response['Instances'][0]['InstanceId'] 

    return {
        'statusCode': 200,
        'body': {
            "instance_id": instance_id,
        }
    }
