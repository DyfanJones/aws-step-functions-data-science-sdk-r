# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/tests/unit/test_service_steps.py

library(mockery)


test_that("test sns publish step creation", {
  stub(get_aws_partition, "get_region", "us-east-1", depth = 2)

  step = SnsPublishStep$new('Publish to SNS', parameters=list(
    'TopicArn'='arn:aws:sns:us-east-1:123456789012:myTopic',
    'Message'='message')
  )

  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        'TopicArn'='arn:aws:sns:us-east-1:123456789012:myTopic',
        'Message'='message'),
      'Resource'='arn:aws:states:::sns:publish',
      'Type'='Task',
      'End'=TRUE
    )
  )

  step = SnsPublishStep$new('Publish to SNS', wait_for_callback=TRUE, parameters=list(
    'TopicArn'='arn:aws:sns:us-east-1:123456789012:myTopic',
    'Message'=list(
      'Input.$'='$',
      'TaskToken.$'='$$.Task.Token')
  ))

  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        'TopicArn'='arn:aws:sns:us-east-1:123456789012:myTopic',
        'Message'=list(
          'Input.$'='$',
          'TaskToken.$'='$$.Task.Token')
        ),
      'Resource'='arn:aws:states:::sns:publish.waitForTaskToken',
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test sqs send message step creation", {
  stub(get_aws_partition, "get_region", "us-east-1", depth = 2)

  step = SqsSendMessageStep$new('Send to SQS', parameters=list(
    'QueueUrl'='https://sqs.us-east-1.amazonaws.com/123456789012/myQueue',
    'MessageBody'='Hello'
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        'QueueUrl'='https://sqs.us-east-1.amazonaws.com/123456789012/myQueue',
        'MessageBody'='Hello'),
      'Resource'='arn:aws:states:::sqs:sendMessage',
      'Type'='Task',
      'End'=TRUE
    )
  )

  step = SqsSendMessageStep$new('Send to SQS', wait_for_callback=TRUE, parameters=list(
    'QueueUrl'='https://sqs.us-east-1.amazonaws.com/123456789012/myQueue',
    'MessageBody'=list(
      'Input.$'='$',
      'TaskToken.$'='$$.Task.Token')
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        'QueueUrl'='https://sqs.us-east-1.amazonaws.com/123456789012/myQueue',
        'MessageBody'=list(
          'Input.$'='$',
          'TaskToken.$'='$$.Task.Token')
      ),
      'Resource'='arn:aws:states:::sqs:sendMessage.waitForTaskToken',
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test dynamodb get item step creation", {
  stub(get_aws_partition, "get_region", "us-east-1", depth = 2)

  step = DynamoDBGetItemStep$new('Read Message From DynamoDB', parameters=list(
    'TableName'='TransferDataRecords-DDBTable-3I41R5L5EAGT',
    'Key'=list(
      'MessageId'=list(
        'S.$'='$.List[0]')
    )
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        'TableName'='TransferDataRecords-DDBTable-3I41R5L5EAGT',
        'Key'=list(
          'MessageId'=list(
            'S.$'='$.List[0]')
        )),
      'Resource'='arn:aws:states:::dynamodb:getItem',
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test dynamodb put item step creation", {
  stub(get_aws_partition, "get_region", "us-east-1", depth = 2)

  step = DynamoDBPutItemStep$new('Put item in DynamoDB', parameters=list(
    'TableName'='TransferDataRecords-DDBTable-3I41R5L5EAGT',
    'Item'=list(
      'MessageId'=list(
        'S'='123456789')
    )
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        'TableName'='TransferDataRecords-DDBTable-3I41R5L5EAGT',
        'Item'=list(
          'MessageId'=list(
            'S'='123456789')
        )),
      'Resource'='arn:aws:states:::dynamodb:putItem',
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test dynamodb delete item step creation", {
  stub(get_aws_partition, "get_region", "us-east-1", depth = 2)

  step = DynamoDBDeleteItemStep$new('Delete item in DynamoDB', parameters=list(
    'TableName'='TransferDataRecords-DDBTable-3I41R5L5EAGT',
    'Key'=list(
      'MessageId'=list(
        'S'='MyMessage')
    )
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        'TableName'='TransferDataRecords-DDBTable-3I41R5L5EAGT',
        'Key'=list(
          'MessageId'=list(
            'S'='MyMessage')
        )),
      'Resource'='arn:aws:states:::dynamodb:deleteItem',
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test dynamodb delete item step creation", {
  stub(get_aws_partition, "get_region", "us-east-1", depth = 2)

  step = DynamoDBUpdateItemStep$new('Update item in DynamoDB', parameters=list(
    'TableName'='TransferDataRecords-DDBTable-3I41R5L5EAGT',
    'Key'=list(
      'RecordId'=list(
        'S'='RecordId')
    ),
    'UpdateExpression'='set Revision = :val1',
    'ExpressionAttributeValues'=list(
      ':val1'=list('S'='2')
    )
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        'TableName'='TransferDataRecords-DDBTable-3I41R5L5EAGT',
        'Key'=list(
          'RecordId'=list(
            'S'='RecordId')
        ),
        'UpdateExpression'='set Revision = :val1',
        'ExpressionAttributeValues'=list(
          ':val1'=list('S'='2')
          )),
      'Resource'='arn:aws:states:::dynamodb:updateItem',
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test emr create cluster step creation", {
  stub(get_aws_partition, "get_region", "us-east-1", depth = 2)

  step = EmrCreateClusterStep$new('Create EMR cluster', parameters=list(
    'Name'='MyWorkflowCluster',
    'VisibleToAllUsers'=TRUE,
    'ReleaseLabel'='emr-5.28.0',
    'Applications'=list(
      list('Name'='Hive')
      ),
    'ServiceRole'='EMR_DefaultRole',
    'JobFlowRole'='EMR_EC2_DefaultRole',
    'LogUri'='s3n://aws-logs-123456789012-us-east-1/elasticmapreduce/',
    'Instances'=list(
      'KeepJobFlowAliveWhenNoSteps'=TRUE,
      'InstanceFleets'=list(
        list(
          'InstanceFleetType'='MASTER',
          'Name'='MASTER',
          'TargetOnDemandCapacity'=1,
          'InstanceTypeConfigs'=list(
            list('InstanceType'='m4.xlarge')
          )
        ),
        list(
          'InstanceFleetType'='CORE',
          'Name'='CORE',
          'TargetOnDemandCapacity'=1,
          'InstanceTypeConfigs'=list(
            list('InstanceType'='m4.xlarge')
          )
        )
      )
    )
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        'Name'='MyWorkflowCluster',
        'VisibleToAllUsers'=TRUE,
        'ReleaseLabel'='emr-5.28.0',
        'Applications'=list(
          list(
            'Name'='Hive')
        ),
        'ServiceRole'='EMR_DefaultRole',
        'JobFlowRole'='EMR_EC2_DefaultRole',
        'LogUri'='s3n://aws-logs-123456789012-us-east-1/elasticmapreduce/',
        'Instances'=list(
          'KeepJobFlowAliveWhenNoSteps'=TRUE,
          'InstanceFleets'=list(
            list(
              'InstanceFleetType'='MASTER',
              'Name'='MASTER',
              'TargetOnDemandCapacity'=1,
              'InstanceTypeConfigs'=list(
                list(
                  'InstanceType'='m4.xlarge')
              )
            ),
            list(
              'InstanceFleetType'='CORE',
              'Name'='CORE',
              'TargetOnDemandCapacity'=1,
              'InstanceTypeConfigs'=list(
                list(
                  'InstanceType'='m4.xlarge')
              )
            )
          )
        )),
      'Resource'='arn:aws:states:::elasticmapreduce:createCluster.sync',
      'Type'='Task',
      'End'=TRUE
    )
  )

  step = EmrCreateClusterStep$new('Create EMR cluster', wait_for_completion=FALSE, parameters=list(
    'Name'='MyWorkflowCluster',
    'VisibleToAllUsers'=TRUE,
    'ReleaseLabel'='emr-5.28.0',
    'Applications'=list(
      list('Name'='Hive')
      ),
    'ServiceRole'='EMR_DefaultRole',
    'JobFlowRole'='EMR_EC2_DefaultRole',
    'LogUri'='s3n://aws-logs-123456789012-us-east-1/elasticmapreduce/',
    'Instances'=list(
      'KeepJobFlowAliveWhenNoSteps'=TRUE,
      'InstanceFleets'=list(
        list(
          'InstanceFleetType'='MASTER',
          'Name'='MASTER',
          'TargetOnDemandCapacity'=1,
          'InstanceTypeConfigs'=list(
            list('InstanceType'='m4.xlarge')
          )
        ),
        list(
          'InstanceFleetType'='CORE',
          'Name'='CORE',
          'TargetOnDemandCapacity'=1,
          'InstanceTypeConfigs'=list(
            list('InstanceType'='m4.xlarge')
          )
        )
      )
    )
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        'Name'='MyWorkflowCluster',
        'VisibleToAllUsers'=TRUE,
        'ReleaseLabel'='emr-5.28.0',
        'Applications'=list(
          list('Name'='Hive')
        ),
        'ServiceRole'='EMR_DefaultRole',
        'JobFlowRole'='EMR_EC2_DefaultRole',
        'LogUri'='s3n://aws-logs-123456789012-us-east-1/elasticmapreduce/',
        'Instances'=list(
          'KeepJobFlowAliveWhenNoSteps'=TRUE,
          'InstanceFleets'=list(
            list(
              'InstanceFleetType'='MASTER',
              'Name'='MASTER',
              'TargetOnDemandCapacity'=1,
              'InstanceTypeConfigs'=list(
                list('InstanceType'='m4.xlarge')
              )
            ),
            list(
              'InstanceFleetType'='CORE',
              'Name'='CORE',
              'TargetOnDemandCapacity'=1,
              'InstanceTypeConfigs'=list(
                list('InstanceType'='m4.xlarge')
              )
            )
          )
        )),
      'Resource'='arn:aws:states:::elasticmapreduce:createCluster',
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test emr terminate cluster step creation", {
  stub(get_aws_partition, "get_region", "us-east-1", depth = 2)

  step = EmrTerminateClusterStep$new('Terminate EMR cluster', parameters=list(
    'ClusterId'='MyWorkflowClusterId'
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        'ClusterId'='MyWorkflowClusterId'
        ),
      'Resource'='arn:aws:states:::elasticmapreduce:terminateCluster.sync',
      'Type'='Task',
      'End'=TRUE
    )
  )

  step = EmrTerminateClusterStep$new('Terminate EMR cluster', wait_for_completion=FALSE, parameters=list(
    'ClusterId'='MyWorkflowClusterId'
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        'ClusterId'='MyWorkflowClusterId'
      ),
      'Resource'='arn:aws:states:::elasticmapreduce:terminateCluster',
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test emr add step step creation", {
  stub(get_aws_partition, "get_region", "us-east-1", depth = 2)

  step = EmrAddStepStep$new('Add step to EMR cluster', parameters=list(
    'ClusterId'='MyWorkflowClusterId',
    'Step'=list(
      'Name'='The first step',
      'ActionOnFailure'='CONTINUE',
      'HadoopJarStep'=list(
        'Jar'='command-runner.jar',
        'Args'=list(
          'hive-script',
          '--run-hive-script',
          '--args',
          '-f',
          's3://<region>.elasticmapreduce.samples/cloudfront/code/Hive_CloudFront.q',
          '-d',
          'INPUT=s3://<region>.elasticmapreduce.samples',
          '-d',
          'OUTPUT=s3://<mybucket>/MyHiveQueryResults/'
        )
      )
    )
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        'ClusterId'='MyWorkflowClusterId',
        'Step'=list(
          'Name'='The first step',
          'ActionOnFailure'='CONTINUE',
          'HadoopJarStep'=list(
            'Jar'='command-runner.jar',
            'Args'=list(
              'hive-script',
              '--run-hive-script',
              '--args',
              '-f',
              's3://<region>.elasticmapreduce.samples/cloudfront/code/Hive_CloudFront.q',
              '-d',
              'INPUT=s3://<region>.elasticmapreduce.samples',
              '-d',
              'OUTPUT=s3://<mybucket>/MyHiveQueryResults/'
            )
          )
        )
      ),
      'Resource'='arn:aws:states:::elasticmapreduce:addStep.sync',
      'Type'='Task',
      'End'=TRUE
    )
  )

  step = EmrAddStepStep$new('Add step to EMR cluster', wait_for_completion=FALSE, parameters=list(
    'ClusterId'='MyWorkflowClusterId',
    'Step'=list(
      'Name'='The first step',
      'ActionOnFailure'='CONTINUE',
      'HadoopJarStep'=list(
        'Jar'='command-runner.jar',
        'Args'=list(
          'hive-script',
          '--run-hive-script',
          '--args',
          '-f',
          's3://<region>.elasticmapreduce.samples/cloudfront/code/Hive_CloudFront.q',
          '-d',
          'INPUT=s3://<region>.elasticmapreduce.samples',
          '-d',
          'OUTPUT=s3://<mybucket>/MyHiveQueryResults/'
        )
      )
    )
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        'ClusterId'='MyWorkflowClusterId',
        'Step'=list(
          'Name'='The first step',
          'ActionOnFailure'='CONTINUE',
          'HadoopJarStep'=list(
            'Jar'='command-runner.jar',
            'Args'=list(
              'hive-script',
              '--run-hive-script',
              '--args',
              '-f',
              's3://<region>.elasticmapreduce.samples/cloudfront/code/Hive_CloudFront.q',
              '-d',
              'INPUT=s3://<region>.elasticmapreduce.samples',
              '-d',
              'OUTPUT=s3://<mybucket>/MyHiveQueryResults/'
            )
          )
        )
      ),
      'Resource'='arn:aws:states:::elasticmapreduce:addStep',
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test emr cancel step step creation", {
  stub(get_aws_partition, "get_region", "us-east-1", depth = 2)

  step = EmrCancelStepStep$new('Cancel step to EMR cluster', parameters=list(
    'ClusterId'='MyWorkflowClusterId',
    'StepId'='MyWorkflowStepId'
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        'ClusterId'='MyWorkflowClusterId',
        'StepId'='MyWorkflowStepId'
      ),
      'Resource'='arn:aws:states:::elasticmapreduce:cancelStep',
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test emr set cluster termination protection step creation", {
  stub(get_aws_partition, "get_region", "us-east-1", depth = 2)

  step = EmrSetClusterTerminationProtectionStep$new('Set termination protection for EMR cluster', parameters=list(
    'ClusterId'='MyWorkflowClusterId',
    'TerminationProtected'=TRUE
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        'ClusterId'='MyWorkflowClusterId',
        'TerminationProtected'=TRUE
      ),
      'Resource'='arn:aws:states:::elasticmapreduce:setClusterTerminationProtection',
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test emr modify instance fleet by name step creation", {
  stub(get_aws_partition, "get_region", "us-east-1", depth = 2)

  step = EmrModifyInstanceFleetByNameStep$new('Modify Instance Fleet by name for EMR cluster', parameters=list(
    'ClusterId'='MyWorkflowClusterId',
    'InstanceFleetName'='MyCoreFleet',
    'InstanceFleet'=list(
      'TargetOnDemandCapacity'=8,
      'TargetSpotCapacity'=0
    )
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        'ClusterId'='MyWorkflowClusterId',
        'InstanceFleetName'='MyCoreFleet',
        'InstanceFleet'=list(
          'TargetOnDemandCapacity'=8,
          'TargetSpotCapacity'=0
        )
      ),
      'Resource'='arn:aws:states:::elasticmapreduce:modifyInstanceFleetByName',
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test emr modify instance group by name step creation", {
  stub(get_aws_partition, "get_region", "us-east-1", depth = 2)

  step = EmrModifyInstanceGroupByNameStep$new('Modify Instance Group by name for EMR cluster', parameters=list(
    'ClusterId'='MyWorkflowClusterId',
    'InstanceGroupName'='MyCoreGroup',
    'InstanceGroup'=list(
      'InstanceCount'=8
    )
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        'ClusterId'='MyWorkflowClusterId',
        'InstanceGroupName'='MyCoreGroup',
        'InstanceGroup'=list(
          'InstanceCount'=8
        )
      ),
      'Resource'='arn:aws:states:::elasticmapreduce:modifyInstanceGroupByName',
      'Type'='Task',
      'End'=TRUE
    )
  )
})

