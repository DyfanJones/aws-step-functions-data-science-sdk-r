# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/tests/unit/test_compute_steps.py

old_env <- Sys.getenv("AWS_REGION")
Sys.setenv("AWS_REGION" = "us-east-1")
test_that("test lambda step creation", {

  step = LambdaStep$new('Echo')

  expect_equal(step$to_list(), list(
    Resource="arn:aws:states:::lambda:invoke",
    Type="Task",
    End=TRUE
    )
  )

  step = LambdaStep$new('lambda', wait_for_callback=TRUE, parameters=list(
    'Payload'=list(
      'model.$'='$.new_model',
      'token.$'='$$.Task.Token')
    )
  )

  expect_equal(step$to_list(), list(
    'Parameters'=list(
      'Payload'=list(
        'model.$'='$.new_model',
        'token.$'='$$.Task.Token')
      ),
    'Resource'='arn:aws:states:::lambda:invoke.waitForTaskToken',
    'Type'='Task',
    'End'=TRUE
    )
  )
})

test_that("test glue start job run step creation",{

  step = GlueStartJobRunStep$new('Glue Job', wait_for_completion=FALSE)

  expect_equal(step$to_list(),list(
    'Resource'='arn:aws:states:::glue:startJobRun',
    'Type'='Task',
    'End'=TRUE)
  )

  step = GlueStartJobRunStep$new('Glue Job', parameters=list(
    'JobName'='Job'
  ))

  expect_equal(step$to_list(),list(
    'Parameters'=list(
      'JobName'='Job'),
    'Resource'='arn:aws:states:::glue:startJobRun.sync',
    'Type'='Task',
    'End'=TRUE)
  )
})

test_that("test batch submit job step creation", {

  step = BatchSubmitJobStep$new('Batch Job', wait_for_completion=FALSE)

  expect_equal(step$to_list(),list(
    'Resource'='arn:aws:states:::batch:submitJob',
    'Type'='Task',
    'End'=TRUE)
  )

  step = BatchSubmitJobStep$new('Batch Job', parameters=list(
    'JobName'='Job',
    'JobQueue'='JobQueue')
  )

  expect_equal(step$to_list(),list(
    'Parameters'=list(
      'JobName'='Job',
      'JobQueue'='JobQueue'),
    'Resource'='arn:aws:states:::batch:submitJob.sync',
    'Type'='Task',
    'End'=TRUE)
  )
})

test_that("test ecs run task step creation", {

  step = EcsRunTaskStep$new('Ecs Job', wait_for_completion=FALSE)

  expect_equal(step$to_list(),list(
    'Resource'='arn:aws:states:::ecs:runTask',
    'Type'='Task',
    'End'=TRUE)
  )

  step = EcsRunTaskStep$new('Batch Job', parameters=list(
    'TaskDefinition'='Task')
  )

  expect_equal(step$to_list(),list(
    'Parameters'=list(
      'TaskDefinition'='Task'),
    'Resource'='arn:aws:states:::ecs:runTask.sync',
    'Type'='Task',
    'End'=TRUE)
  )
})

test_that("test databrew start job task step creation", {

  step = DataBrewStartJobRunStep$new('DataBrew Job', wait_for_completion=FALSE)

  expect_equal(step$to_list(),list(
    'Resource'='arn:aws:states:::databrew:startJobRun',
    'Type'='Task',
    'End'=TRUE)
  )

  step = DataBrewStartJobRunStep$new('Batch Job', parameters=list(
    "Name"="sample-proj-job-1")
  )

  expect_equal(step$to_list(),list(
    'Parameters'=list(
      "Name"="sample-proj-job-1"),
    'Resource'='arn:aws:states:::databrew:startJobRun.sync',
    'Type'='Task',
    'End'=TRUE)
  )
})

test_that("test athena start query execution task step creation", {

  step = AthenaStartQueryExecutionStep$new('Athena Job', wait_for_completion=FALSE)

  expect_equal(step$to_list(),list(
    'Resource'='arn:aws:states:::athena:startQueryExecution',
    'Type'='Task',
    'End'=TRUE)
  )

  step = AthenaStartQueryExecutionStep$new('Athena Job', parameters=list(
    "QueryString"='SELECT * FROM "myDatabase"."myTable" limit 1',
    "WorkGroup"="primary",
    "ResultConfiguration"=list(
    "OutputLocation"="s3://athenaQueryResult")
    )
  )

  expect_equal(step$to_list(),list(
    'Parameters'=list(
      "QueryString"='SELECT * FROM "myDatabase"."myTable" limit 1',
      "WorkGroup"="primary",
      "ResultConfiguration"=list(
        "OutputLocation"="s3://athenaQueryResult")
    ),
    'Resource'='arn:aws:states:::athena:startQueryExecution.sync',
    'Type'='Task',
    'End'=TRUE)
  )
})

test_that("test athena stop query execution task step creation", {

  step = AthenaStopQueryExecutionStep$new('Athena Job')

  expect_equal(step$to_list(),list(
    'Resource'='arn:aws:states:::athena:stopQueryExecution',
    'Type'='Task',
    'End'=TRUE)
  )

  step = AthenaStopQueryExecutionStep$new('Athena Job', parameters=list(
    "QueryExecutionId.$"="$.QueryExecution.QueryExecutionId")
  )

  expect_equal(step$to_list(),list(
    'Parameters'=list("QueryExecutionId.$"="$.QueryExecution.QueryExecutionId"),
    'Resource'='arn:aws:states:::athena:stopQueryExecution',
    'Type'='Task',
    'End'=TRUE)
  )
})

test_that("test athena get query execution task step creation", {

  step = AthenaGetQueryExecutionStep$new('Athena Job')

  expect_equal(step$to_list(),list(
    'Resource'='arn:aws:states:::athena:getQueryExecution',
    'Type'='Task',
    'End'=TRUE)
  )

  step = AthenaGetQueryExecutionStep$new('Athena Job', parameters=list(
    "QueryExecutionId.$"="$.QueryExecution.QueryExecutionId")
  )

  expect_equal(step$to_list(),list(
    'Parameters'=list("QueryExecutionId.$"="$.QueryExecution.QueryExecutionId"),
    'Resource'='arn:aws:states:::athena:getQueryExecution',
    'Type'='Task',
    'End'=TRUE)
  )
})

test_that("test athena get query results task step creation", {

  step = AthenaGetQueryResultsStep$new('Athena Job')

  expect_equal(step$to_list(),list(
    'Resource'='arn:aws:states:::athena:getQueryResults',
    'Type'='Task',
    'End'=TRUE)
  )

  step = AthenaGetQueryResultsStep$new('Athena Job', parameters=list(
    "QueryExecutionId.$"="$.QueryExecution.QueryExecutionId")
  )

  expect_equal(step$to_list(),list(
    'Parameters'=list("QueryExecutionId.$"="$.QueryExecution.QueryExecutionId"),
    'Resource'='arn:aws:states:::athena:getQueryResults',
    'Type'='Task',
    'End'=TRUE)
  )
})

Sys.setenv("AWS_REGION"=old_env)
