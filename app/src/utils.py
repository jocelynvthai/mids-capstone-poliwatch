import boto3


def set_db_connection():

    s3 = boto3.resource(
        service_name='s3',
        region_name='us-east-1',
        aws_access_key_id='AKIAUBVWHVMYZ6DB3MWH',
        aws_secret_access_key='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'
    )

    return s3
