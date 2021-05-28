# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/tests/unit/test_service_steps.py

old_env <- Sys.getenv("AWS_REGION")
Sys.setenv("AWS_REGION" = "us-east-1")
test_that("test sns publish step creation", {
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

test_that("test code build start build step creation", {


  step = CodeBuildStartBuildStep$new('Start build for CodeBuild', parameters=list(
    "projectName"="dummy"
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "projectName"="dummy"
      ),
      'Resource'="arn:aws:states:::codebuild:startBuild.sync",
      'Type'='Task',
      'End'=TRUE
    )
  )

  step = CodeBuildStartBuildStep$new('Start build for CodeBuild', wait_for_completion=F, parameters=list(
    "projectName"="dummy"
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "projectName"="dummy"
      ),
      'Resource'="arn:aws:states:::codebuild:startBuild",
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test code build stop build step creation", {


  step = CodeBuildStopBuildStep$new('Start build for CodeBuild', parameters=list(
    "projectName"="dummy"
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "projectName"="dummy"
      ),
      'Resource'="arn:aws:states:::codebuild:stopBuild",
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test code build stop build step creation", {


  step = CodeBuildStopBuildStep$new('Stop build for CodeBuild', parameters=list(
    "id"="dummy"
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "id"="dummy"
      ),
      'Resource'="arn:aws:states:::codebuild:stopBuild",
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test code build batch delete builds step creation", {


  step = CodeBuildBatchDeleteBuildsStep$new('Delete builds for CodeBuild', parameters=list(
    "ids"=list("dummy")
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "ids"=list("dummy")
      ),
      'Resource'="arn:aws:states:::codebuild:batchDeleteBuilds",
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test code build batch get reports step creation", {


  step = CodeBuildBatchGetReportsStep$new('Get report for CodeBuild', parameters=list(
    "reportArns"=list("arn::dummy")
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "reportArns"=list("arn::dummy")
      ),
      'Resource'="arn:aws:states:::codebuild:batchGetReports",
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test event bridge put events step creation", {


  step = EventBridgePutEventsStep$new('Put event in Event Bridge', parameters=list(
    "Entries"=list(
      list(
        "Detail"=list("Message"="MyMessage"),
        "DetailType"="MyDetailType",
        "EventBusName"="MyEventBus",
        "Source"="my.source"
      )
    )
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "Entries"=list(
          list(
            "Detail"=list("Message"="MyMessage"),
            "DetailType"="MyDetailType",
            "EventBusName"="MyEventBus",
            "Source"="my.source"
          )
        )
      ),
      'Resource'="arn:aws:states:::events:putEvents",
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test stepfunction start execution step creation", {


  step = StepfunctionsStartExecutionStep$new('Start step function execution', parameters=list(
    "Input"=list("Comment"="Hello world!"),
    "StateMachineArn"="arn:aws:states:us-east-1:123456789012:stateMachine:HelloWorld",
    "Name"="ExecutionName"
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "Input"=list("Comment"="Hello world!"),
        "StateMachineArn"="arn:aws:states:us-east-1:123456789012:stateMachine:HelloWorld",
        "Name"="ExecutionName"
      ),
      'Resource'="arn:aws:states:::states:startExecution.sync",
      'Type'='Task',
      'End'=TRUE
    )
  )

  step = StepfunctionsStartExecutionStep$new('Start step function execution', wait_for_completion=F, parameters=list(
    "Input"=list("Comment"="Hello world!"),
    "StateMachineArn"="arn:aws:states:us-east-1:123456789012:stateMachine:HelloWorld",
    "Name"="ExecutionName"
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "Input"=list("Comment"="Hello world!"),
        "StateMachineArn"="arn:aws:states:us-east-1:123456789012:stateMachine:HelloWorld",
        "Name"="ExecutionName"
      ),
      'Resource'="arn:aws:states:::states:startExecution",
      'Type'='Task',
      'End'=TRUE
    )
  )

  step = StepfunctionsStartExecutionStep$new('Start step function execution', wait_for_completion=F, wait_for_callback=T, parameters=list(
    "Input"=list(
      "Comment"="Hello world!",
      "token.$"="$$.Task.Token"),
    "StateMachineArn"="arn:aws:states:us-east-1:123456789012:stateMachine:HelloWorld",
    "Name"="ExecutionName"
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "Input"=list(
          "Comment"="Hello world!",
          "token.$"="$$.Task.Token"),
        "StateMachineArn"="arn:aws:states:us-east-1:123456789012:stateMachine:HelloWorld",
        "Name"="ExecutionName"
      ),
      'Resource'="arn:aws:states:::states:startExecution.waitForTaskToken",
      'Type'='Task',
      'End'=TRUE
    )
  )

  expect_error(
    step = StepfunctionsStartExecutionStep$new('Start step function execution', wait_for_callback=T, parameters=list(
      "Input"=list(
        "Comment"="Hello world!",
        "token.$"="$$.Task.Token"),
      "StateMachineArn"="arn:aws:states:us-east-1:123456789012:stateMachine:HelloWorld",
      "Name"="ExecutionName"
    ))
  )
})

test_that("test api gateway step step creation", {


  step = ApiGatewayStep$new('Invoke  Api Gateway', parameters=list(
    "ApiEndpoint"="example.execute-api.us-east-1.amazonaws.com",
    "Method"="GET",
    "Headers"=list(
      "key"=list("value1", "value2")),
    "Stage"="prod",
    "Path"="bills",
    "QueryParameters"=list(
      "billId"=list("123456")
    ),
    "RequestBody"=list(),
    "AuthType"="NO_AUTH"
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "ApiEndpoint"="example.execute-api.us-east-1.amazonaws.com",
        "Method"="GET",
        "Headers"=list(
          "key"=list("value1", "value2")),
        "Stage"="prod",
        "Path"="bills",
        "QueryParameters"=list(
          "billId"=list("123456")
        ),
        "RequestBody"=list(),
        "AuthType"="NO_AUTH"
      ),
      'Resource'="arn:aws:states:::apigateway:invoke",
      'Type'='Task',
      'End'=TRUE
    )
  )

  step = ApiGatewayStep$new('Invoke Api Gateway', wait_for_callback=T, parameters=list(
    "ApiEndpoint"="example.execute-api.us-east-1.amazonaws.com",
    "Method"="POST",
    "Headers"=list("TaskToken.$"="States.Array($$.Task.Token)"),
    "Stage"="prod",
    "Path"="bills/add",
    "QueryParameters"=list(),
    "RequestBody"=list("billId"="my-new-bill"),
    "AuthType"="IAM_AUTH"
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "ApiEndpoint"="example.execute-api.us-east-1.amazonaws.com",
        "Method"="POST",
        "Headers"=list("TaskToken.$"="States.Array($$.Task.Token)"),
        "Stage"="prod",
        "Path"="bills/add",
        "QueryParameters"=list(),
        "RequestBody"=list("billId"="my-new-bill"),
        "AuthType"="IAM_AUTH"
      ),
      'Resource'="arn:aws:states:::apigateway:invoke.waitForTaskToken",
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test emr on eks create virtual cluster step creation", {


  step = EmrOnEksCreateVirtualClusterStep$new('Create Cluster', parameters=list(
    "Name"="MyVirtualCluster",
    "ContainerProvider"=list(
      "Id"="EKSClusterName",
      "Type"="EKS",
      "Info"=list(
        "EksInfo"=list("Namespace"="Namespace")
      )
    )
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "Name"="MyVirtualCluster",
        "ContainerProvider"=list(
          "Id"="EKSClusterName",
          "Type"="EKS",
          "Info"=list(
            "EksInfo"=list("Namespace"="Namespace")
          )
        )
      ),
      'Resource'="arn:aws:states:::emr-containers:createVirtualCluster",
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test emr on eks delete virtual cluster step creation", {


  step = EmrOnEksDeleteVirtualClusterStep$new('Delete Cluster', parameters=list(
    "Id.$"="$.VirtualClusterId"
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "Id.$"="$.VirtualClusterId"
      ),
      'Resource'="arn:aws:states:::emr-containers:deleteVirtualCluster.sync",
      'Type'='Task',
      'End'=TRUE
    )
  )

  step = EmrOnEksDeleteVirtualClusterStep$new('Delete Cluster', wait_for_completion=F, parameters=list(
    "Id.$"="$.VirtualClusterId"
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "Id.$"="$.VirtualClusterId"
      ),
      'Resource'="arn:aws:states:::emr-containers:deleteVirtualCluster",
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test emr on eks start job step creation", {


  step = EmrOnEksStartJobRunStep$new('Start Job on Eks Cluster', parameters=list(
    "Name"="MyJobName",
    "VirtualClusterId.$"="$.VirtualClusterId",
    "ExecutionRoleArn"="arn:aws:iam::<accountId>:role/job-execution-role",
    "ReleaseLabel"="emr-6.2.0-latest",
    "JobDriver"=list(
      "SparkSubmitJobDriver"=list(
        "EntryPoint"="s3://<mybucket>/jobs/trip-count.py",
        "EntryPointArguments"=list(
          "60"
        ),
        "SparkSubmitParameters"="--conf spark.driver.cores=2 --conf spark.executor.instances=10 --conf spark.kubernetes.pyspark.pythonVersion=3 --conf spark.executor.memory=10G --conf spark.driver.memory=10G --conf spark.executor.cores=1 --conf spark.dynamicAllocation.enabled=false"
      )
    ),
    "ConfigurationOverrides"=list(
      "ApplicationConfiguration"=list(
        list(
          "Classification"="spark-defaults",
          "Properties"=list(
            "spark.executor.instances"="2",
            "spark.executor.memory"="2G"
          )
        )
      ),
      "MonitoringConfiguration"=list(
        "PersistentAppUI"="ENABLED",
        "CloudWatchMonitoringConfiguration"=list(
          "LogGroupName"="MyLogGroupName",
          "LogStreamNamePrefix"="MyLogStreamNamePrefix"
        ),
        "S3MonitoringConfiguration"=list(
          "LogUri"="s3://<mylogsbucket>"
        )
      )
    )
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "Name"="MyJobName",
        "VirtualClusterId.$"="$.VirtualClusterId",
        "ExecutionRoleArn"="arn:aws:iam::<accountId>:role/job-execution-role",
        "ReleaseLabel"="emr-6.2.0-latest",
        "JobDriver"=list(
          "SparkSubmitJobDriver"=list(
            "EntryPoint"="s3://<mybucket>/jobs/trip-count.py",
            "EntryPointArguments"=list(
              "60"
            ),
            "SparkSubmitParameters"="--conf spark.driver.cores=2 --conf spark.executor.instances=10 --conf spark.kubernetes.pyspark.pythonVersion=3 --conf spark.executor.memory=10G --conf spark.driver.memory=10G --conf spark.executor.cores=1 --conf spark.dynamicAllocation.enabled=false"
          )
        ),
        "ConfigurationOverrides"=list(
          "ApplicationConfiguration"=list(
            list(
              "Classification"="spark-defaults",
              "Properties"=list(
                "spark.executor.instances"="2",
                "spark.executor.memory"="2G"
              )
            )
          ),
          "MonitoringConfiguration"=list(
            "PersistentAppUI"="ENABLED",
            "CloudWatchMonitoringConfiguration"=list(
              "LogGroupName"="MyLogGroupName",
              "LogStreamNamePrefix"="MyLogStreamNamePrefix"
            ),
            "S3MonitoringConfiguration"=list(
              "LogUri"="s3://<mylogsbucket>"
            )
          )
        )
      ),
      'Resource'="arn:aws:states:::emr-containers:startJobRun.sync",
      'Type'='Task',
      'End'=TRUE
    )
  )

  step = EmrOnEksStartJobRunStep$new('Start Job on Eks Cluster', wait_for_completion=F, parameters=list(
    "Name"="MyJobName",
    "VirtualClusterId.$"="$.VirtualClusterId",
    "ExecutionRoleArn"="arn:aws:iam::<accountId>:role/job-execution-role",
    "ReleaseLabel"="emr-6.2.0-latest",
    "JobDriver"=list(
      "SparkSubmitJobDriver"=list(
        "EntryPoint"="s3://<mybucket>/jobs/trip-count.py",
        "EntryPointArguments"=list(
          "60"
        ),
        "SparkSubmitParameters"="--conf spark.driver.cores=2 --conf spark.executor.instances=10 --conf spark.kubernetes.pyspark.pythonVersion=3 --conf spark.executor.memory=10G --conf spark.driver.memory=10G --conf spark.executor.cores=1 --conf spark.dynamicAllocation.enabled=false"
      )
    ),
    "ConfigurationOverrides"=list(
      "ApplicationConfiguration"=list(
        list(
          "Classification"="spark-defaults",
          "Properties"=list(
            "spark.executor.instances"="2",
            "spark.executor.memory"="2G"
          )
        )
      ),
      "MonitoringConfiguration"=list(
        "PersistentAppUI"="ENABLED",
        "CloudWatchMonitoringConfiguration"=list(
          "LogGroupName"="MyLogGroupName",
          "LogStreamNamePrefix"="MyLogStreamNamePrefix"
        ),
        "S3MonitoringConfiguration"=list(
          "LogUri"="s3://<mylogsbucket>"
        )
      )
    )
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "Name"="MyJobName",
        "VirtualClusterId.$"="$.VirtualClusterId",
        "ExecutionRoleArn"="arn:aws:iam::<accountId>:role/job-execution-role",
        "ReleaseLabel"="emr-6.2.0-latest",
        "JobDriver"=list(
          "SparkSubmitJobDriver"=list(
            "EntryPoint"="s3://<mybucket>/jobs/trip-count.py",
            "EntryPointArguments"=list(
              "60"
            ),
            "SparkSubmitParameters"="--conf spark.driver.cores=2 --conf spark.executor.instances=10 --conf spark.kubernetes.pyspark.pythonVersion=3 --conf spark.executor.memory=10G --conf spark.driver.memory=10G --conf spark.executor.cores=1 --conf spark.dynamicAllocation.enabled=false"
          )
        ),
        "ConfigurationOverrides"=list(
          "ApplicationConfiguration"=list(
            list(
              "Classification"="spark-defaults",
              "Properties"=list(
                "spark.executor.instances"="2",
                "spark.executor.memory"="2G"
              )
            )
          ),
          "MonitoringConfiguration"=list(
            "PersistentAppUI"="ENABLED",
            "CloudWatchMonitoringConfiguration"=list(
              "LogGroupName"="MyLogGroupName",
              "LogStreamNamePrefix"="MyLogStreamNamePrefix"
            ),
            "S3MonitoringConfiguration"=list(
              "LogUri"="s3://<mylogsbucket>"
            )
          )
        )
      ),
      'Resource'="arn:aws:states:::emr-containers:startJobRun",
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test eks run job step creation", {


  step = EksRunJobStep$new('Run Job', parameters=list(
    "ClusterName"="MyCluster",
    "CertificateAuthority"="ANPAJ2UCCR6DPCEXAMPLE",
    "Endpoint"="https://AKIAIOSFODNN7EXAMPLE.yl4.us-east-1.eks.amazonaws.com",
    "LogOptions"=list("RetrieveLogs"=TRUE),
    "Job"=list(
      "apiVersion"="batch/v1",
      "kind"="Job",
      "metadata"=list(
        "name"="example-job"
      ),
      "spec"=list(
        "backoffLimit"=0,
        "template"=list(
          "metadata"=list("name"="example-job"),
          "spec"=list(
            "containers"=list(
              list(
                "name"="pi-2000",
                "image"="perl",
                "command"=list("perl"),
                "args"=list(
                  "-Mbignum=bpi",
                  "-wle",
                  "print bpi(2000)"
                )
              )
            ),
            "restartPolicy"="Never"
          )
        )
      )
    )
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "ClusterName"="MyCluster",
        "CertificateAuthority"="ANPAJ2UCCR6DPCEXAMPLE",
        "Endpoint"="https://AKIAIOSFODNN7EXAMPLE.yl4.us-east-1.eks.amazonaws.com",
        "LogOptions"=list("RetrieveLogs"=TRUE),
        "Job"=list(
          "apiVersion"="batch/v1",
          "kind"="Job",
          "metadata"=list(
            "name"="example-job"
          ),
          "spec"=list(
            "backoffLimit"=0,
            "template"=list(
              "metadata"=list("name"="example-job"),
              "spec"=list(
                "containers"=list(
                  list(
                    "name"="pi-2000",
                    "image"="perl",
                    "command"=list("perl"),
                    "args"=list(
                      "-Mbignum=bpi",
                      "-wle",
                      "print bpi(2000)"
                    )
                  )
                ),
                "restartPolicy"="Never"
              )
            )
          )
        )
      ),
      'Resource'="arn:aws:states:::eks:runJob.sync",
      'Type'='Task',
      'End'=TRUE
    )
  )

  step = EksRunJobStep$new('Run Job', wait_for_completion=F, parameters=list(
    "ClusterName"="MyCluster",
    "CertificateAuthority"="ANPAJ2UCCR6DPCEXAMPLE",
    "Endpoint"="https://AKIAIOSFODNN7EXAMPLE.yl4.us-east-1.eks.amazonaws.com",
    "LogOptions"=list("RetrieveLogs"=TRUE),
    "Job"=list(
      "apiVersion"="batch/v1",
      "kind"="Job",
      "metadata"=list(
        "name"="example-job"
      ),
      "spec"=list(
        "backoffLimit"=0,
        "template"=list(
          "metadata"=list("name"="example-job"),
          "spec"=list(
            "containers"=list(
              list(
                "name"="pi-2000",
                "image"="perl",
                "command"=list("perl"),
                "args"=list(
                  "-Mbignum=bpi",
                  "-wle",
                  "print bpi(2000)"
                )
              )
            ),
            "restartPolicy"="Never"
          )
        )
      )
    )
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "ClusterName"="MyCluster",
        "CertificateAuthority"="ANPAJ2UCCR6DPCEXAMPLE",
        "Endpoint"="https://AKIAIOSFODNN7EXAMPLE.yl4.us-east-1.eks.amazonaws.com",
        "LogOptions"=list("RetrieveLogs"=TRUE),
        "Job"=list(
          "apiVersion"="batch/v1",
          "kind"="Job",
          "metadata"=list(
            "name"="example-job"
          ),
          "spec"=list(
            "backoffLimit"=0,
            "template"=list(
              "metadata"=list("name"="example-job"),
              "spec"=list(
                "containers"=list(
                  list(
                    "name"="pi-2000",
                    "image"="perl",
                    "command"=list("perl"),
                    "args"=list(
                      "-Mbignum=bpi",
                      "-wle",
                      "print bpi(2000)"
                    )
                  )
                ),
                "restartPolicy"="Never"
              )
            )
          )
        )
      ),
      'Resource'="arn:aws:states:::eks:runJob",
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test eks call step creation", {


  step = EksCallStep$new('Call Eks', parameters=list(
    "ClusterName"="MyCluster",
    "CertificateAuthority"="ANPAJ2UCCR6DPCEXAMPLE",
    "Endpoint"="https://444455556666.yl4.us-east-1.eks.amazonaws.com",
    "Method"="GET",
    "Path"="/api/v1/namespaces/default/pods",
    "QueryParameters"=list(
      "labelSelector"=list("job-name=example-job")
    )
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "ClusterName"="MyCluster",
        "CertificateAuthority"="ANPAJ2UCCR6DPCEXAMPLE",
        "Endpoint"="https://444455556666.yl4.us-east-1.eks.amazonaws.com",
        "Method"="GET",
        "Path"="/api/v1/namespaces/default/pods",
        "QueryParameters"=list(
          "labelSelector"=list("job-name=example-job")
        )
      ),
      'Resource'="arn:aws:states:::eks:call",
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test eks create cluster step creation", {


  step = EksCreateClusterStep$new('CreateCluster', parameters=list(
    "Name"="MyCluster",
    "ResourcesVpcConfig"=list(
      "SubnetIds"=list(
        "subnet-053e7c47012341234",
        "subnet-027cfea4b12341234")
    ),
    "RoleArn"="arn:aws:iam::123456789012:role/MyEKSClusterRole"
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "Name"="MyCluster",
        "ResourcesVpcConfig"=list(
          "SubnetIds"=list(
            "subnet-053e7c47012341234",
            "subnet-027cfea4b12341234")
        ),
        "RoleArn"="arn:aws:iam::123456789012:role/MyEKSClusterRole"
      ),
      'Resource'="arn:aws:states:::eks:createCluster.sync",
      'Type'='Task',
      'End'=TRUE
    )
  )

  step = EksCreateClusterStep$new('CreateCluster', wait_for_completion=F, parameters=list(
    "Name"="MyCluster",
    "ResourcesVpcConfig"=list(
      "SubnetIds"=list(
        "subnet-053e7c47012341234",
        "subnet-027cfea4b12341234")
    ),
    "RoleArn"="arn:aws:iam::123456789012:role/MyEKSClusterRole"
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "Name"="MyCluster",
        "ResourcesVpcConfig"=list(
          "SubnetIds"=list(
            "subnet-053e7c47012341234",
            "subnet-027cfea4b12341234")
        ),
        "RoleArn"="arn:aws:iam::123456789012:role/MyEKSClusterRole"
      ),
      'Resource'="arn:aws:states:::eks:createCluster",
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test eks delete cluster step creation", {


  step = EksDeleteClusterStep$new('DeleteCluster', parameters=list(
    "Name"="MyCluster"
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "Name"="MyCluster"
      ),
      'Resource'="arn:aws:states:::eks:deleteCluster.sync",
      'Type'='Task',
      'End'=TRUE
    )
  )

  step = EksDeleteClusterStep$new('DeleteCluster', wait_for_completion=F, parameters=list(
    "Name"="MyCluster"
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "Name"="MyCluster"
      ),
      'Resource'="arn:aws:states:::eks:deleteCluster",
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test eks create fargate profile step creation", {


  step = EksCreateFargateProfileStep$new('create fargate profile', parameters=list(
    "ClusterName"="MyCluster",
    "FargateProfileName"="MyFargateProfile",
    "PodExecutionRoleArn"="arn:aws:iam::123456789012:role/MyFargatePodExecutionRole",
    "Selectors"=list(list(
      "Namespace"="my-namespace",
      "Labels"=list("my-label"="my-value")
    ))
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "ClusterName"="MyCluster",
        "FargateProfileName"="MyFargateProfile",
        "PodExecutionRoleArn"="arn:aws:iam::123456789012:role/MyFargatePodExecutionRole",
        "Selectors"=list(list(
          "Namespace"="my-namespace",
          "Labels"=list("my-label"="my-value")
        ))
      ),
      'Resource'="arn:aws:states:::eks:createFargateProfile.sync",
      'Type'='Task',
      'End'=TRUE
    )
  )

  step = EksCreateFargateProfileStep$new('create fargate profile', wait_for_completion=F, parameters=list(
    "ClusterName"="MyCluster",
    "FargateProfileName"="MyFargateProfile",
    "PodExecutionRoleArn"="arn:aws:iam::123456789012:role/MyFargatePodExecutionRole",
    "Selectors"=list(list(
      "Namespace"="my-namespace",
      "Labels"=list("my-label"="my-value")
    ))
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "ClusterName"="MyCluster",
        "FargateProfileName"="MyFargateProfile",
        "PodExecutionRoleArn"="arn:aws:iam::123456789012:role/MyFargatePodExecutionRole",
        "Selectors"=list(list(
          "Namespace"="my-namespace",
          "Labels"=list("my-label"="my-value")
        ))
      ),
      'Resource'="arn:aws:states:::eks:createFargateProfile",
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test eks delete fargate profile step creation", {


  step = EksDeleteFargateProfileStep$new('delete fargate profile', parameters=list(
    "ClusterName"="MyCluster",
    "FargateProfileName"="MyFargateProfile"
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "ClusterName"="MyCluster",
        "FargateProfileName"="MyFargateProfile"
      ),
      'Resource'="arn:aws:states:::eks:deleteFargateProfile.sync",
      'Type'='Task',
      'End'=TRUE
    )
  )

  step = EksDeleteFargateProfileStep$new('delete fargate profile', wait_for_completion=F, parameters=list(
    "ClusterName"="MyCluster",
    "FargateProfileName"="MyFargateProfile"
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "ClusterName"="MyCluster",
        "FargateProfileName"="MyFargateProfile"
      ),
      'Resource'="arn:aws:states:::eks:deleteFargateProfile",
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test eks create node group step creation", {


  step = EksCreateNodegroupStep$new('create node group', parameters=list(
    "ClusterName"="MyCluster",
    "NodegroupName"="MyNodegroup",
    "NodeRole"="arn:aws:iam::123456789012:role/MyNodeInstanceRole",
    "Subnets"=list("subnet-09fb51df01234", "subnet-027cfea4b1234")
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "ClusterName"="MyCluster",
        "NodegroupName"="MyNodegroup",
        "NodeRole"="arn:aws:iam::123456789012:role/MyNodeInstanceRole",
        "Subnets"=list("subnet-09fb51df01234", "subnet-027cfea4b1234")
      ),
      'Resource'="arn:aws:states:::eks:createNodegroup.sync",
      'Type'='Task',
      'End'=TRUE
    )
  )

  step = EksCreateNodegroupStep$new('create node group', wait_for_completion=F, parameters=list(
    "ClusterName"="MyCluster",
    "NodegroupName"="MyNodegroup",
    "NodeRole"="arn:aws:iam::123456789012:role/MyNodeInstanceRole",
    "Subnets"=list("subnet-09fb51df01234", "subnet-027cfea4b1234")
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "ClusterName"="MyCluster",
        "NodegroupName"="MyNodegroup",
        "NodeRole"="arn:aws:iam::123456789012:role/MyNodeInstanceRole",
        "Subnets"=list("subnet-09fb51df01234", "subnet-027cfea4b1234")
      ),
      'Resource'="arn:aws:states:::eks:createNodegroup",
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test eks delete node group step creation", {


  step = EksDeleteNodegroupStep$new('delete node group', parameters=list(
    "ClusterName"="MyCluster",
    "NodegroupName"="MyNodegroup"
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "ClusterName"="MyCluster",
        "NodegroupName"="MyNodegroup"
      ),
      'Resource'="arn:aws:states:::eks:deleteNodegroup.sync",
      'Type'='Task',
      'End'=TRUE
    )
  )

  step = EksDeleteNodegroupStep$new('delete node group', wait_for_completion=F, parameters=list(
    "ClusterName"="MyCluster",
    "NodegroupName"="MyNodegroup"
  ))
  expect_equal(
    step$to_list(),
    list(
      'Parameters'=list(
        "ClusterName"="MyCluster",
        "NodegroupName"="MyNodegroup"
      ),
      'Resource'="arn:aws:states:::eks:deleteNodegroup",
      'Type'='Task',
      'End'=TRUE
    )
  )
})

Sys.setenv("AWS_REGION"=old_env)
