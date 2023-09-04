import os
import boto3

ec2 = boto3.client('ec2')

def has_runner_tag(instance, tag_key):
    return any(tag['Key'] == tag_key for tag in instance['Tags'])

def bad_request(reason):
    return {
        'statusCode': 400,
        'body': {
            'reason': reason
        }
    }
    
def lambda_handler(event, context):
    tag_key = os.environ.get('TAG_KEY')

    if 'instance_id' not in event:
        return bad_request("No instance_id provided")

    instances = ec2.describe_instances(
        InstanceIds=[event['instance_id']],
        Filters=[
            {
                'Name': 'instance-state-name',
                'Values': ['running'],
            }
        ],
    )

    if not len(instances['Reservations']) == 1:
        return bad_request("Too many or no reservations")

    if not len(instances['Reservations'][0]['Instances']) == 1:
        return bad_request("Too many or no instances")

    instance = instances['Reservations'][0]['Instances'][0]

    if not has_runner_tag(instance, tag_key):
        return bad_request("Instance is not tagged")

    ec2.terminate_instances(
        InstanceIds=[instance['InstanceId']]
    )

    return {
        'statusCode': 200,
        'body': {
            "instance_id": instance['InstanceId']
        }
    }