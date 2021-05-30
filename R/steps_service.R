# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/src/stepfunctions/steps/service.py

#' @include steps_fields.R
#' @include steps_states.R
#' @include utils.R
#' @include steps_integrate_resources.R

#' @import R6

DYNAMODB_SERVICE_NAME = "dynamodb"
SNS_SERVICE_NAME = "sns"
SQS_SERVICE_NAME = "sqs"
ELASTICMAPREDUCE_SERVICE_NAME = "elasticmapreduce"
CODEBUILD_SERVICE_NAME = "codebuild"
EVENTBRIDGE_SERVICE_NAME = "events"
STEPFUNCTIONS_SERVICE_NAME = "states"
APIGATEWAY_SERVICE_NAME = "apigateway"
EMRONEKS_SERVICE_NAME = "emr-containers"
EKS_SERVICE_NAME = "eks"

DynamoDBApi = Enum(
  GetItem = "getItem",
  PutItem = "putItem",
  DeleteItem = "deleteItem",
  UpdateItem = "updateItem"
)

SnsApi = Enum(
  Publish = "publish"
)

SqsApi = Enum(
  SendMessage = "sendMessage"
)

ElasticMapReduceApi = Enum(
  CreateCluster = "createCluster",
  TerminateCluster = "terminateCluster",
  AddStep = "addStep",
  CancelStep = "cancelStep",
  SetClusterTerminationProtection = "setClusterTerminationProtection",
  ModifyInstanceFleetByName = "modifyInstanceFleetByName",
  ModifyInstanceGroupByName = "modifyInstanceGroupByName"
)

CodeBuildApi = Enum(
  StartBuild = "startBuild",
  StopBuild = "stopBuild",
  BatchDeleteBuilds = "batchDeleteBuilds",
  BatchGetReports = "batchGetReports"
)

EventBridgeApi = Enum(
  PutEvents = "putEvents"
)

StepfunctionsApi = Enum(
  StartExecution = "startExecution"
)

ApiGatewayApi = Enum(
  Invoke = "invoke"
)

EmrOnEksApi = Enum(
  CreateVirtualCluster = "createVirtualCluster",
  DeleteVirtualCluster = "deleteVirtualCluster",
  StartJobRun = "startJobRun"
)

EksApi = Enum(
  RunJob = "runJob",
  Call = "call",
  CreateCluster = "createCluster",
  DeleteCluster = "deleteCluster",
  CreateFargateProfile = "createFargateProfile",
  DeleteFargateProfile = "deleteFargateProfile",
  CreateNodegroup = "createNodegroup",
  DeleteNodegroup = "deleteNodegroup"
)

#' @title DynamoDBGetItemStep class
#' @description Creates a Task state to get an item from DynamoDB. See `Call DynamoDB`
#'              APIs with Step Functions \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-ddb.html}
#'              for more details.
#' @export
DynamoDBGetItemStep = R6Class("DynamoDBGetItemStep",
  inherit = Task,
  public = list(

    #' @description Initialize DynamoDBGetItemStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'               to 128 unicode characters. State names **must be** unique within
    #'               the scope of the whole state machine.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = c(as.list(environment()), list(...))
      kwargs[[Field$Resource]] = get_service_integration_arn(
        DYNAMODB_SERVICE_NAME,
        DynamoDBApi$GetItem)
      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title DynamoDBPutItemStep class
#' @description Creates a Task state to put an item to DynamoDB. See `Call DynamoDB`
#'              APIs with Step Functions \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-ddb.html}
#'              for more details.
#' @export
DynamoDBPutItemStep = R6Class("DynamoDBPutItemStep",
  inherit = Task,
  public = list(

    #' @description Initialize DynamoDBPutItemStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'               to 128 unicode characters. State names **must be** unique within
    #'               the scope of the whole state machine.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = c(as.list(environment()), list(...))
      kwargs[[Field$Resource]] = get_service_integration_arn(
        DYNAMODB_SERVICE_NAME,
        DynamoDBApi$PutItem)
      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title DynamoDBDeleteItemStep class
#' @description Creates a Task state to delete an item from DynamoDB. See Call DynamoDB
#'              APIs with Step Functions \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-ddb.html}
#'              for more details.
#' @export
DynamoDBDeleteItemStep = R6Class("DynamoDBDeleteItemStep",
  inherit = Task,
  public = list(
    #' @description Initialize DynamoDBDeleteItemStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'               to 128 unicode characters. State names **must be** unique within
    #'               the scope of the whole state machine.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = c(as.list(environment()), list(...))
      kwargs[[Field$Resource]] = get_service_integration_arn(
        DYNAMODB_SERVICE_NAME,
        DynamoDBApi$DeleteItem)
      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title DynamoDBUpdateItemStep class
#' @description Creates a Task state to update an item from DynamoDB. See Call DynamoDB
#'              APIs with Step Functions \url{ttps://docs.aws.amazon.com/step-functions/latest/dg/connect-ddb.html}
#'              for more details.
#' @export
DynamoDBUpdateItemStep = R6Class("DynamoDBUpdateItemStep",
  inherit = Task,
  public = list(

    #' @description Initialize DynamoDBUpdateItemStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'              to 128 unicode characters. State names **must be** unique within
    #'              the scope of the whole state machine.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = c(as.list(environment()), list(...))
      kwargs[[Field$Resource]] = get_service_integration_arn(
        DYNAMODB_SERVICE_NAME,
        DynamoDBApi$UpdateItem)
      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title SnsPublishStep class
#' @description Creates a Task state to publish a message to SNS topic. See Call Amazon SNS
#'              with Step Functions \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-sns.html}
#'              for more details.
#' @export
SnsPublishStep = R6Class("SnsPublishStep",
  inherit = Task,
  public = list(

    #' @description Initialize SnsPublishStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'               to 128 unicode characters. State names **must be** unique within
    #'               the scope of the whole state machine.
    #' @param wait_for_callback (bool, optional): Boolean value set to `True` if the
    #'               Task state should wait for callback to resume the operation. (default: False)
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          wait_for_callback=FALSE,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = list(
        state_id=state_id,
        timeout_seconds=timeout_seconds,
        timeout_seconds_path=timeout_seconds_path,
        heartbeat_seconds=heartbeat_seconds,
        heartbeat_seconds_path=heartbeat_seconds_path,
        comment=comment,
        input_path=input_path,
        parameters=parameters,
        result_path=result_path,
        output_path=output_path,
        ...)

      if (wait_for_callback)
        kwargs[[Field$Resource]] = get_service_integration_arn(
          SNS_SERVICE_NAME,
          SnsApi$Publish,
          IntegrationPattern$WaitForTaskToken)
      else
        kwargs[[Field$Resource]] = get_service_integration_arn(
          SNS_SERVICE_NAME,
          SnsApi$Publish)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title SqsSendMessageStep class
#' @description Creates a Task state to send a message to SQS queue. See Call Amazon SQS
#'              with Step Functions \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-sqs.html}
#'              for more details.
#' @export
SqsSendMessageStep = R6Class("SqsSendMessageStep",
  inherit = Task,
  public = list(

    #' @description Initialize SqsSendMessageStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'               to 128 unicode characters. State names **must be** unique within
    #'               the scope of the whole state machine.
    #' @param wait_for_callback (bool, optional): Boolean value set to `True` if the
    #'               Task state should wait for callback to resume the operation. (default: False)
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          wait_for_callback=FALSE,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = list(
        state_id=state_id,
        timeout_seconds=timeout_seconds,
        timeout_seconds_path=timeout_seconds_path,
        heartbeat_seconds=heartbeat_seconds,
        heartbeat_seconds_path=heartbeat_seconds_path,
        comment=comment,
        input_path=input_path,
        parameters=parameters,
        result_path=result_path,
        output_path=output_path,
        ...)

      if (wait_for_callback)
        kwargs[[Field$Resource]] = get_service_integration_arn(
          SQS_SERVICE_NAME,
          SqsApi$SendMessage,
          IntegrationPattern$WaitForTaskToken)
      else
        kwargs[[Field$Resource]] = get_service_integration_arn(
          SQS_SERVICE_NAME,
          SqsApi$SendMessage)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EmrCreateClusterStep class
#' @description Creates a Task state to create and start running a cluster (job flow). See
#'              Call Amazon EMR with Step Functions
#'              \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-emr.html}
#'              for more details.
#' @export
EmrCreateClusterStep = R6Class("EmrCreateClusterStep",
  inherit = Task,
  public = list(

    #' @description Initialize EmrCreateClusterStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'              to 128 unicode characters. State names **must be** unique within
    #'              the scope of the whole state machine.
    #' @param wait_for_completion (bool, optional): Boolean value set to `True` if
    #'              the Task state should wait to complete before proceeding to the
    #'              next step in the workflow. (default: True)
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          wait_for_completion=TRUE,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = list(
       state_id=state_id,
       timeout_seconds=timeout_seconds,
       timeout_seconds_path=timeout_seconds_path,
       heartbeat_seconds=heartbeat_seconds,
       heartbeat_seconds_path=heartbeat_seconds_path,
       comment=comment,
       input_path=input_path,
       parameters=parameters,
       result_path=result_path,
       output_path=output_path,
       ...)

      if (wait_for_completion)
        kwargs[[Field$Resource]] = get_service_integration_arn(
          ELASTICMAPREDUCE_SERVICE_NAME,
          ElasticMapReduceApi$CreateCluster,
          IntegrationPattern$WaitForCompletion)
      else
        kwargs[[Field$Resource]] = get_service_integration_arn(
          ELASTICMAPREDUCE_SERVICE_NAME,
          ElasticMapReduceApi$CreateCluster)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EmrTerminateClusterStep class
#' @description Creates a Task state to shut down a cluster (job flow). See Call Amazon EMR
#'              with Step Functions \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-emr.html}
#'              for more details.
#' @export
EmrTerminateClusterStep = R6Class("EmrTerminateClusterStep",
  inherit = Task,
  public = list(

    #' @description Initialize EmrTerminateClusterStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'              to 128 unicode characters. State names **must be** unique within
    #'              the scope of the whole state machine.
    #' @param wait_for_completion (bool, optional): Boolean value set to `True` if
    #'              the Task state should wait to complete before proceeding to the
    #'              next step in the workflow. (default: True)
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          wait_for_completion=TRUE,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = list(
        state_id=state_id,
        timeout_seconds=timeout_seconds,
        timeout_seconds_path=timeout_seconds_path,
        heartbeat_seconds=heartbeat_seconds,
        heartbeat_seconds_path=heartbeat_seconds_path,
        comment=comment,
        input_path=input_path,
        parameters=parameters,
        result_path=result_path,
        output_path=output_path,
        ...)

      if (wait_for_completion)
        kwargs[[Field$Resource]] = get_service_integration_arn(
          ELASTICMAPREDUCE_SERVICE_NAME,
          ElasticMapReduceApi$TerminateCluster,
          IntegrationPattern$WaitForCompletion)
      else
        kwargs[[Field$Resource]] = get_service_integration_arn(
          ELASTICMAPREDUCE_SERVICE_NAME,
          ElasticMapReduceApi$TerminateCluster)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EmrAddStepStep class
#' @description Creates a Task state to shut down a cluster (job flow). See Call Amazon EMR
#'              with Step Functions \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-emr.html}
#'              for more details.
#' @export
EmrAddStepStep = R6Class("EmrAddStepStep",
  inherit = Task,
  public = list(

    #' @description Initialize EmrAddStepStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'              to 128 unicode characters. State names **must be** unique within
    #'              the scope of the whole state machine.
    #' @param wait_for_completion (bool, optional): Boolean value set to `True` if
    #'              the Task state should wait to complete before proceeding to the
    #'              next step in the workflow. (default: True)
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          wait_for_completion=TRUE,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = list(
        state_id=state_id,
        timeout_seconds=timeout_seconds,
        timeout_seconds_path=timeout_seconds_path,
        heartbeat_seconds=heartbeat_seconds,
        heartbeat_seconds_path=heartbeat_seconds_path,
        comment=comment,
        input_path=input_path,
        parameters=parameters,
        result_path=result_path,
        output_path=output_path,
        ...)

      if (wait_for_completion)
        kwargs[[Field$Resource]] = get_service_integration_arn(
          ELASTICMAPREDUCE_SERVICE_NAME,
          ElasticMapReduceApi$AddStep,
          IntegrationPattern$WaitForCompletion)
      else
        kwargs[[Field$Resource]] =  get_service_integration_arn(
          ELASTICMAPREDUCE_SERVICE_NAME,
          ElasticMapReduceApi$AddStep)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EmrCancelStepStep class
#' @description Creates a Task state to cancel a pending step in a running cluster. See
#'             Call Amazon EMR with Step Functions
#'             \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-emr.html}
#'             for more details.
#' @export
EmrCancelStepStep = R6Class("EmrCancelStepStep",
  inherit = Task,
  public = list(

    #' @description Initialize EmrCancelStepStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'              to 128 unicode characters. State names **must be** unique within
    #'              the scope of the whole state machine.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = c(as.list(environment()), list(...))
      kwargs[[Field$Resource]] = get_service_integration_arn(
        ELASTICMAPREDUCE_SERVICE_NAME,
        ElasticMapReduceApi$CancelStep)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EmrSetClusterTerminationProtectionStep class
#' @description Creates a Task state to lock a cluster (job flow) so the EC2 instances
#'              in the cluster cannot be terminated by user intervention, an API call, or
#'              a job-flow error. See Call Amazon EMR with Step Functions
#'              \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-emr.html} for more details.
#' @export
EmrSetClusterTerminationProtectionStep = R6Class("EmrSetClusterTerminationProtectionStep",
  inherit = Task,
  public = list(

    #' @description Initialize EmrSetClusterTerminationProtectionStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'              to 128 unicode characters. State names **must be** unique within
    #'              the scope of the whole state machine.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = c(as.list(environment()), list(...))
      kwargs[[Field$Resource]] = get_service_integration_arn(
        ELASTICMAPREDUCE_SERVICE_NAME,
        ElasticMapReduceApi$SetClusterTerminationProtection)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EmrModifyInstanceFleetByNameStep class
#' @description Creates a Task state to modify the target On-Demand and target Spot capacities
#'              for an instance fleet.  See Call Amazon EMR with Step Functions
#'              \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-emr.html} for more details.
#' @export
EmrModifyInstanceFleetByNameStep = R6Class("EmrModifyInstanceFleetByNameStep",
  inherit = Task,
  public = list(

    #' @description Initialize EmrModifyInstanceFleetByNameStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'              to 128 unicode characters. State names **must be** unique within
    #'              the scope of the whole state machine.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = c(as.list(environment()), list(...))
      kwargs[[Field$Resource]] = get_service_integration_arn(
        ELASTICMAPREDUCE_SERVICE_NAME,
        ElasticMapReduceApi$ModifyInstanceFleetByName)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EmrModifyInstanceGroupByNameStep class
#' @description Creates a Task state to modify the number of nodes and configuration
#'              settings of an instance group. See Call Amazon EMR with Step Functions
#'              \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-emr.html} for more details.
#' @export
EmrModifyInstanceGroupByNameStep = R6Class("EmrModifyInstanceGroupByNameStep",
  inherit = Task,
  public = list(

    #' @description Initialize EmrModifyInstanceGroupByNameStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'              to 128 unicode characters. State names **must be** unique within
    #'              the scope of the whole state machine.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = c(as.list(environment()), list(...))
      kwargs[[Field$Resource]] = get_service_integration_arn(
        ELASTICMAPREDUCE_SERVICE_NAME,
        ElasticMapReduceApi$ModifyInstanceGroupByName)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title CodeBuildStartBuildStep class
#' @description Creates a Task state to start code build jobs. See Call Amazon Code Build
#'              with Step Functions \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-codebuild.html}
#'              for more details.
#' @export
CodeBuildStartBuildStep = R6Class("CodeBuildStartBuildStep",
  inherit = Task,
  public = list(

    #' @description Initialize CodeBuildStartBuildStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'               to 128 unicode characters. State names **must be** unique within
    #'               the scope of the whole state machine.
    #' @param wait_for_completion (bool, optional): Boolean value set to `True` if
    #'              the Task state should wait for the glue job to complete before proceeding
    #'              to the next step in the workflow. Set to `False` if the Task state should
    #'              submit the glue job and proceed to the next step. (default: True)
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          wait_for_completion=TRUE,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = list(
      state_id=state_id,
      timeout_seconds=timeout_seconds,
      timeout_seconds_path=timeout_seconds_path,
      heartbeat_seconds=heartbeat_seconds,
      heartbeat_seconds_path=heartbeat_seconds_path,
      comment=comment,
      input_path=input_path,
      parameters=parameters,
      result_path=result_path,
      output_path=output_path,
      ...)

      if (wait_for_completion)
        kwargs[[Field$Resource]] = get_service_integration_arn(
          CODEBUILD_SERVICE_NAME,
          CodeBuildApi$StartBuild,
          IntegrationPattern$WaitForCompletion)
      else
        kwargs[[Field$Resource]] = get_service_integration_arn(
          CODEBUILD_SERVICE_NAME,
          CodeBuildApi$StartBuild)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title CodeBuildStopBuildStep class
#' @description Creates a Task state to stop code build jobs. See Call Amazon Code Build
#'              with Step Functions \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-codebuild.html}
#'              for more details.
#' @export
CodeBuildStopBuildStep = R6Class("CodeBuildStopBuildStep",
  inherit = Task,
  public = list(

    #' @description Initialize CodeBuildStopBuildStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'               to 128 unicode characters. State names **must be** unique within
    #'               the scope of the whole state machine.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = c(as.list(environment()), list(...))

      kwargs[[Field$Resource]] = get_service_integration_arn(
        CODEBUILD_SERVICE_NAME,
        CodeBuildApi$StopBuild)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title CodeBuildBatchDeleteBuildsStep class
#' @description Creates a Task state to delete batch builds. See Call Amazon Code Build
#'              with Step Functions \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-codebuild.html}
#'              for more details.
#' @export
CodeBuildBatchDeleteBuildsStep = R6Class("CodeBuildBatchDeleteBuildsStep",
  inherit = Task,
  public = list(

    #' @description Initialize CodeBuildBatchDeleteBuildsStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'               to 128 unicode characters. State names **must be** unique within
    #'               the scope of the whole state machine.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = c(as.list(environment()), list(...))

      kwargs[[Field$Resource]] = get_service_integration_arn(
        CODEBUILD_SERVICE_NAME,
        CodeBuildApi$BatchDeleteBuilds)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title CodeBuildBatchGetReportsStep class
#' @description Creates a Task state to get reports. See Call Amazon Code Build
#'              with Step Functions \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-codebuild.html}
#'              for more details.
#' @export
CodeBuildBatchGetReportsStep = R6Class("CodeBuildBatchGetReportsStep",
  inherit = Task,
  public = list(

    #' @description Initialize CodeBuildBatchGetReportsStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'               to 128 unicode characters. State names **must be** unique within
    #'               the scope of the whole state machine.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = c(as.list(environment()), list(...))

      kwargs[[Field$Resource]] = get_service_integration_arn(
        CODEBUILD_SERVICE_NAME,
        CodeBuildApi$BatchGetReports)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EventBridgePutEventsStep class
#' @description Creates a Task state to sends custom events to Amazon EventBridge
#'              with Step Functions \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-eventbridge.html}
#'              for more details.
#' @export
EventBridgePutEventsStep = R6Class("EventBridgePutEventsStep",
  inherit = Task,
  public = list(

    #' @description Initialize EventBridgePutEventsStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'               to 128 unicode characters. State names **must be** unique within
    #'               the scope of the whole state machine.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = c(as.list(environment()), list(...))

      kwargs[[Field$Resource]] = get_service_integration_arn(
        EVENTBRIDGE_SERVICE_NAME,
        EventBridgeApi$PutEvents)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title StepfunctionsStartExecutionStep class
#' @description Creates a Task state to start a new Step Functions
#'              execution with Step Functions \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-stepfunctions.html}
#'              for more details.
#' @note This class only calls another stepfunction state machine from the "parent" state machine.
#' @export
StepfunctionsStartExecutionStep = R6Class("StepfunctionsStartExecutionStep",
  inherit = Task,
  public = list(

    #' @description Initialize SnsPublishStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'               to 128 unicode characters. State names **must be** unique within
    #'               the scope of the whole state machine.
    #' @param wait_for_completion (bool, optional): Boolean value set to `True` if
    #'              the Task state should wait for the glue job to complete before proceeding
    #'              to the next step in the workflow. Set to `False` if the Task state should
    #'              submit the glue job and proceed to the next step. (default: True)
    #' @param wait_for_callback (bool, optional): Boolean value set to `True` if the
    #'               Task state should wait for callback to resume the operation. (default: False)
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          wait_for_completion=TRUE,
                          wait_for_callback=FALSE,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = list(
        state_id=state_id,
        timeout_seconds=timeout_seconds,
        timeout_seconds_path=timeout_seconds_path,
        heartbeat_seconds=heartbeat_seconds,
        heartbeat_seconds_path=heartbeat_seconds_path,
        comment=comment,
        input_path=input_path,
        parameters=parameters,
        result_path=result_path,
        output_path=output_path,
        ...)

      if(wait_for_completion && wait_for_callback)
        stop("Both `wait_for_completion` and `wait_for_callback` are set to `TRUE`")

      if (wait_for_completion) {
        kwargs[[Field$Resource]] = get_service_integration_arn(
          STEPFUNCTIONS_SERVICE_NAME,
          StepfunctionsApi$StartExecution,
          IntegrationPattern$WaitForCompletion)
      } else if (wait_for_callback) {
        kwargs[[Field$Resource]] = get_service_integration_arn(
          STEPFUNCTIONS_SERVICE_NAME,
          StepfunctionsApi$StartExecution,
          IntegrationPattern$WaitForTaskToken)
      } else {
        kwargs[[Field$Resource]] = get_service_integration_arn(
          STEPFUNCTIONS_SERVICE_NAME,
          StepfunctionsApi$StartExecution)
      }
      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title ApiGatewayStep class
#' @description Creates a Task state to invoke api gateway with Step Functions
#'              \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-api-gateway.html}
#'              for more details.
#' @note This class only calls another stepfunction state machine from the "parent" state machine.
#' @export
ApiGatewayStep = R6Class("ApiGateway",
  inherit = Task,
  public = list(

    #' @description Initialize ApiGatewayStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'               to 128 unicode characters. State names **must be** unique within
    #'               the scope of the whole state machine.
    #' @param wait_for_callback (bool, optional): Boolean value set to `True` if the
    #'               Task state should wait for callback to resume the operation. (default: False)
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          wait_for_callback=FALSE,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = list(
        state_id=state_id,
        timeout_seconds=timeout_seconds,
        timeout_seconds_path=timeout_seconds_path,
        heartbeat_seconds=heartbeat_seconds,
        heartbeat_seconds_path=heartbeat_seconds_path,
        comment=comment,
        input_path=input_path,
        parameters=parameters,
        result_path=result_path,
        output_path=output_path,
        ...)

      if (wait_for_callback) {
        kwargs[[Field$Resource]] = get_service_integration_arn(
          APIGATEWAY_SERVICE_NAME,
          ApiGatewayApi$Invoke,
          IntegrationPattern$WaitForTaskToken)
      } else {
        kwargs[[Field$Resource]] = get_service_integration_arn(
          APIGATEWAY_SERVICE_NAME,
          ApiGatewayApi$Invoke)
      }
      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EmrOnEksCreateVirtualClusterStep class
#' @description Creates a Task state to creates a virtual cluster. See Call Amazon
#'              EMR on EKS with Step Functions \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-emr-eks.html}
#'              for more details.
#' @export
EmrOnEksCreateVirtualClusterStep = R6Class("EmrOnEksCreateVirtualClusterStep",
  inherit = Task,
  public = list(

    #' @description Initialize EmrOnEksCreateVirtualClusterStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'              to 128 unicode characters. State names **must be** unique within
    #'              the scope of the whole state machine.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = c(as.list(environment()), list(...))
      kwargs[[Field$Resource]] = get_service_integration_arn(
        EMRONEKS_SERVICE_NAME,
        EmrOnEksApi$CreateVirtualCluster)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EmrOnEksDeleteVirtualClusterStep class
#' @description Creates a Task state to delete a virtual cluster. See Call Amazon
#'              EMR on EKS with Step Functions \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-emr-eks.html}
#'              for more details.
#' @export
EmrOnEksDeleteVirtualClusterStep = R6Class("EmrOnEksDeleteVirtualClusterStep",
  inherit = Task,
  public = list(

    #' @description Initialize EmrOnEksDeleteVirtualClusterStep task class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'              to 128 unicode characters. State names **must be** unique within
    #'              the scope of the whole state machine.
    #' @param wait_for_completion (bool, optional): Boolean value set to `True` if
    #'              the Task state should wait to complete before proceeding to the
    #'              next step in the workflow. (default: True)
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout
    #'              for the state in seconds. If the state runs longer than the specified
    #'              timeout, then the interpreter fails the state with a `States.Timeout`
    #'              Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must
    #'              select a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          wait_for_completion=TRUE,
                          comment=NULL,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = list(
        state_id=state_id,
        timeout_seconds=timeout_seconds,
        timeout_seconds_path=timeout_seconds_path,
        heartbeat_seconds=heartbeat_seconds,
        heartbeat_seconds_path=heartbeat_seconds_path,
        comment=comment,
        input_path=input_path,
        parameters=parameters,
        result_path=result_path,
        output_path=output_path,
        ...)

      if(wait_for_completion)
        kwargs[[Field$Resource]] = get_service_integration_arn(
          EMRONEKS_SERVICE_NAME,
          EmrOnEksApi$DeleteVirtualCluster,
          IntegrationPattern$WaitForCompletion)
      else
        kwargs[[Field$Resource]] = get_service_integration_arn(
          EMRONEKS_SERVICE_NAME,
          EmrOnEksApi$DeleteVirtualCluster)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EmrOnEksStartJobRunStep class
#' @description Creates a Task state to start job. See Call Amazon
#'              EMR on EKS with Step Functions \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-emr-eks.html}
#'              for more details.
#' @export
EmrOnEksStartJobRunStep = R6Class("EmrOnEksStartJobRunStep",
  inherit = Task,
  public = list(

  #' @description Initialize EmrOnEksStartJobRunStep task class
  #' @param state_id (str): State name whose length **must be** less than or equal
  #'              to 128 unicode characters. State names **must be** unique within
  #'              the scope of the whole state machine.
  #' @param wait_for_completion (bool, optional): Boolean value set to `True` if
  #'              the Task state should wait to complete before proceeding to the
  #'              next step in the workflow. (default: True)
  #' @param comment (str, optional): Human-readable comment or description. (default: None)
  #' @param timeout_seconds (int, optional): Positive integer specifying timeout
  #'              for the state in seconds. If the state runs longer than the specified
  #'              timeout, then the interpreter fails the state with a `States.Timeout`
  #'              Error Name. (default: 60)
  #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
  #'              value in seconds from the state input. When resolved, the path must
  #'              select a field whose value is a positive integer.
  #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
  #'              timeout for the state in seconds. This value should be lower than
  #'              the one specified for `timeout_seconds`. If more time than the specified
  #'              heartbeat elapses between heartbeats from the task, then the interpreter
  #'              fails the state with a `States.Timeout` Error Name.
  #' @param heartbeat_seconds_path (str, optional): Path specifying the state's heartbeat
  #'              value in seconds from the state input. When resolved, the path must select
  #'              a field whose value is a positive integer.
  #' @param input_path (str, optional): Path applied to the state’s raw input to
  #'              select some or all of it; that selection is used by the state. (default: '$')
  #' @param parameters (list, optional): The value of this field becomes the effective
  #'              input for the state.
  #' @param result_path (str, optional): Path specifying the raw input’s combination
  #'              with or replacement by the state’s result. (default: '$')
  #' @param output_path (str, optional): Path applied to the state’s output after
  #'              the application of `result_path`, producing the effective output
  #'              which serves as the raw input for the next state. (default: '$')
  #' @param ... : Extra Fields passed to Task class
  initialize = function(state_id,
                        wait_for_completion=TRUE,
                        comment=NULL,
                        timeout_seconds=NULL,
                        timeout_seconds_path=NULL,
                        heartbeat_seconds=NULL,
                        heartbeat_seconds_path=NULL,
                        input_path=NULL,
                        parameters=NULL,
                        result_path=NULL,
                        output_path=NULL,
                        ...){
    kwargs = list(
      state_id=state_id,
      timeout_seconds=timeout_seconds,
      timeout_seconds_path=timeout_seconds_path,
      heartbeat_seconds=heartbeat_seconds,
      heartbeat_seconds_path=heartbeat_seconds_path,
      comment=comment,
      input_path=input_path,
      parameters=parameters,
      result_path=result_path,
      output_path=output_path,
      ...)

    if(wait_for_completion)
      kwargs[[Field$Resource]] = get_service_integration_arn(
        EMRONEKS_SERVICE_NAME,
        EmrOnEksApi$StartJobRun,
        IntegrationPattern$WaitForCompletion)
    else
      kwargs[[Field$Resource]] = get_service_integration_arn(
        EMRONEKS_SERVICE_NAME,
        EmrOnEksApi$StartJobRun)

    do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EksRunJobStep class
#' @description Creates a Task State to run a job
#'              on your Amazon EKS cluster. See `Manage Amazon EKS` with Step Functions
#'              \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-eks.html}
#'              for more details.
#' @export
EksRunJobStep = R6Class("EksRunJobStep",
  inherit = Task,
  public = list(

    #' @description Initialize EksRunJobStep Task class
    #' @param state_id (str): State name whose length **must be** less than or
    #'              equal to 128 unicode characters. State names **must be** unique
    #'              within the scope of the whole state machine.
    #' @param wait_for_completion (bool, optional): Boolean value set to `True` if
    #'              the Task state should wait for the glue job to complete before proceeding
    #'              to the next step in the workflow. Set to `False` if the Task state should
    #'              submit the glue job and proceed to the next step. (default: True)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout for the
    #'              state in seconds. If the state runs longer than the specified timeout,
    #'              then the interpreter fails the state with a `States.Timeout` Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's
    #'              heartbeat value in seconds from the state input. When resolved,
    #'              the path must select a field whose value is a positive integer.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          wait_for_completion=TRUE,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          comment=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = list(
        state_id=state_id,
        timeout_seconds=timeout_seconds,
        timeout_seconds_path=timeout_seconds_path,
        heartbeat_seconds=heartbeat_seconds,
        heartbeat_seconds_path=heartbeat_seconds_path,
        comment=comment,
        input_path=input_path,
        parameters=parameters,
        result_path=result_path,
        output_path=output_path,
      ...)
      if (wait_for_completion)
        kwargs[Field$Resource] = get_service_integration_arn(
          EKS_SERVICE_NAME,
          EksApi$RunJob,
          IntegrationPattern$WaitForCompletion)
      else
        kwargs[[Field$Resource]] = get_service_integration_arn(
          EKS_SERVICE_NAME,
          EksApi$RunJob)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EksCallStep class
#' @description Creates a Task State to use the Kubernetes API to read and write
#'              Kubernetes resource objects via a Kubernetes API endpoint. See `Manage Amazon EKS` with Step Functions
#'              \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-eks.html}
#'              for more details.
#' @export
EksCallStep = R6Class("EksCallStep",
  inherit = Task,
  public = list(

    #' @description Initialize EksCallStep Task class
    #' @param state_id (str): State name whose length **must be** less than or
    #'              equal to 128 unicode characters. State names **must be** unique
    #'              within the scope of the whole state machine.
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout for the
    #'              state in seconds. If the state runs longer than the specified timeout,
    #'              then the interpreter fails the state with a `States.Timeout` Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's
    #'              heartbeat value in seconds from the state input. When resolved,
    #'              the path must select a field whose value is a positive integer.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          comment=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = c(as.list(environment()), list(...))
      kwargs[[Field$Resource]] = get_service_integration_arn(
        EKS_SERVICE_NAME,
        EksApi$Call)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EksCreateClusterStep class
#' @description Creates a Task State to created `AWS EKS cluster`.
#'              When `AWS EKS` is created the IAM role is added to the `Kubernetes RBAC`
#'              authorization table as the administrator. See `Manage Amazon EKS` with Step Functions
#'              \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-eks.html}
#'              for more details.
#' @export
EksCreateClusterStep = R6Class("EksCreateClusterStep",
  inherit = Task,
  public = list(

    #' @description Initialize EksCreateClusterStep Task class
    #' @param state_id (str): State name whose length **must be** less than or
    #'              equal to 128 unicode characters. State names **must be** unique
    #'              within the scope of the whole state machine.
    #' @param wait_for_completion (bool, optional): Boolean value set to `True` if
    #'              the Task state should wait for the glue job to complete before proceeding
    #'              to the next step in the workflow. Set to `False` if the Task state should
    #'              submit the glue job and proceed to the next step. (default: True)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout for the
    #'              state in seconds. If the state runs longer than the specified timeout,
    #'              then the interpreter fails the state with a `States.Timeout` Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's
    #'              heartbeat value in seconds from the state input. When resolved,
    #'              the path must select a field whose value is a positive integer.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          wait_for_completion=TRUE,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          comment=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = list(
        state_id=state_id,
        timeout_seconds=timeout_seconds,
        timeout_seconds_path=timeout_seconds_path,
        heartbeat_seconds=heartbeat_seconds,
        heartbeat_seconds_path=heartbeat_seconds_path,
        comment=comment,
        input_path=input_path,
        parameters=parameters,
        result_path=result_path,
        output_path=output_path,
        ...)
      if (wait_for_completion)
        kwargs[Field$Resource] = get_service_integration_arn(
          EKS_SERVICE_NAME,
          EksApi$CreateCluster,
          IntegrationPattern$WaitForCompletion)
      else
        kwargs[[Field$Resource]] = get_service_integration_arn(
          EKS_SERVICE_NAME,
          EksApi$CreateCluster)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EksDeleteClusterStep class
#' @description Creates a Task State to delete `AWS EKS cluster`. See `Manage Amazon EKS` with Step Functions
#'              \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-eks.html}
#'              for more details.
#' @note You must delete any Fargate profiles or node groups before deleting a cluster.
#' @export
EksDeleteClusterStep = R6Class("EksDeleteClusterStep",
  inherit = Task,
  public = list(

    #' @description Initialize EksDeleteClusterStep Task class
    #' @param state_id (str): State name whose length **must be** less than or
    #'              equal to 128 unicode characters. State names **must be** unique
    #'              within the scope of the whole state machine.
    #' @param wait_for_completion (bool, optional): Boolean value set to `True` if
    #'              the Task state should wait for the glue job to complete before proceeding
    #'              to the next step in the workflow. Set to `False` if the Task state should
    #'              submit the glue job and proceed to the next step. (default: True)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout for the
    #'              state in seconds. If the state runs longer than the specified timeout,
    #'              then the interpreter fails the state with a `States.Timeout` Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's
    #'              heartbeat value in seconds from the state input. When resolved,
    #'              the path must select a field whose value is a positive integer.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          wait_for_completion=TRUE,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          comment=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = list(
        state_id=state_id,
        timeout_seconds=timeout_seconds,
        timeout_seconds_path=timeout_seconds_path,
        heartbeat_seconds=heartbeat_seconds,
        heartbeat_seconds_path=heartbeat_seconds_path,
        comment=comment,
        input_path=input_path,
        parameters=parameters,
        result_path=result_path,
        output_path=output_path,
        ...)
      if (wait_for_completion)
        kwargs[Field$Resource] = get_service_integration_arn(
          EKS_SERVICE_NAME,
          EksApi$DeleteCluster,
          IntegrationPattern$WaitForCompletion)
      else
        kwargs[[Field$Resource]] = get_service_integration_arn(
          EKS_SERVICE_NAME,
          EksApi$DeleteCluster)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EksCreateFargateProfileStep class
#' @description Creates a Task State to create Fargate Profile on `AWS EKS cluster`.
#'              `Amazon EKS` uses service-linked roles which contain the permissions
#'              `Amazon EKS` requires to call other services on your behalf
#'              See `Manage Amazon EKS` with Step Functions
#'              \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-eks.html}
#'              for more details.
#' @export
EksCreateFargateProfileStep = R6Class("EksCreateFargateProfileStep",
  inherit = Task,
  public = list(

    #' @description Initialize EksCreateFargateProfileStep Task class
    #' @param state_id (str): State name whose length **must be** less than or
    #'              equal to 128 unicode characters. State names **must be** unique
    #'              within the scope of the whole state machine.
    #' @param wait_for_completion (bool, optional): Boolean value set to `True` if
    #'              the Task state should wait for the glue job to complete before proceeding
    #'              to the next step in the workflow. Set to `False` if the Task state should
    #'              submit the glue job and proceed to the next step. (default: True)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout for the
    #'              state in seconds. If the state runs longer than the specified timeout,
    #'              then the interpreter fails the state with a `States.Timeout` Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's
    #'              heartbeat value in seconds from the state input. When resolved,
    #'              the path must select a field whose value is a positive integer.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          wait_for_completion=TRUE,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          comment=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = list(
        state_id=state_id,
        timeout_seconds=timeout_seconds,
        timeout_seconds_path=timeout_seconds_path,
        heartbeat_seconds=heartbeat_seconds,
        heartbeat_seconds_path=heartbeat_seconds_path,
        comment=comment,
        input_path=input_path,
        parameters=parameters,
        result_path=result_path,
        output_path=output_path,
        ...)
      if (wait_for_completion)
        kwargs[Field$Resource] = get_service_integration_arn(
          EKS_SERVICE_NAME,
          EksApi$CreateFargateProfile,
          IntegrationPattern$WaitForCompletion)
      else
        kwargs[[Field$Resource]] = get_service_integration_arn(
          EKS_SERVICE_NAME,
          EksApi$CreateFargateProfile)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EksDeleteFargateProfileStep class
#' @description Creates a Task State to delete Fargate Profile on `AWS EKS cluster`.
#'              See `Manage Amazon EKS` with Step Functions
#'              \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-eks.html}
#'              for more details.
#' @export
EksDeleteFargateProfileStep = R6Class("EksDeleteFargateProfileStep",
  inherit = Task,
  public = list(

    #' @description Initialize EksDeleteFargateProfileStep Task class
    #' @param state_id (str): State name whose length **must be** less than or
    #'              equal to 128 unicode characters. State names **must be** unique
    #'              within the scope of the whole state machine.
    #' @param wait_for_completion (bool, optional): Boolean value set to `True` if
    #'              the Task state should wait for the glue job to complete before proceeding
    #'              to the next step in the workflow. Set to `False` if the Task state should
    #'              submit the glue job and proceed to the next step. (default: True)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout for the
    #'              state in seconds. If the state runs longer than the specified timeout,
    #'              then the interpreter fails the state with a `States.Timeout` Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's
    #'              heartbeat value in seconds from the state input. When resolved,
    #'              the path must select a field whose value is a positive integer.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          wait_for_completion=TRUE,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          comment=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = list(
        state_id=state_id,
        timeout_seconds=timeout_seconds,
        timeout_seconds_path=timeout_seconds_path,
        heartbeat_seconds=heartbeat_seconds,
        heartbeat_seconds_path=heartbeat_seconds_path,
        comment=comment,
        input_path=input_path,
        parameters=parameters,
        result_path=result_path,
        output_path=output_path,
        ...)
      if (wait_for_completion)
        kwargs[Field$Resource] = get_service_integration_arn(
          EKS_SERVICE_NAME,
          EksApi$DeleteFargateProfile,
          IntegrationPattern$WaitForCompletion)
      else
        kwargs[[Field$Resource]] = get_service_integration_arn(
          EKS_SERVICE_NAME,
          EksApi$DeleteFargateProfile)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EksCreateNodegroupStep class
#' @description Creates a Task State to create a managed node group for an `AWS EKS cluster`.
#'              See `Manage Amazon EKS` with Step Functions
#'              \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-eks.html}
#'              for more details.
#' @export
EksCreateNodegroupStep = R6Class("EksCreateNodegroupStep",
  inherit = Task,
  public = list(

    #' @description Initialize EksCreateNodegroupStep Task class
    #' @param state_id (str): State name whose length **must be** less than or
    #'              equal to 128 unicode characters. State names **must be** unique
    #'              within the scope of the whole state machine.
    #' @param wait_for_completion (bool, optional): Boolean value set to `True` if
    #'              the Task state should wait for the glue job to complete before proceeding
    #'              to the next step in the workflow. Set to `False` if the Task state should
    #'              submit the glue job and proceed to the next step. (default: True)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout for the
    #'              state in seconds. If the state runs longer than the specified timeout,
    #'              then the interpreter fails the state with a `States.Timeout` Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's
    #'              heartbeat value in seconds from the state input. When resolved,
    #'              the path must select a field whose value is a positive integer.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          wait_for_completion=TRUE,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          comment=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = list(
        state_id=state_id,
        timeout_seconds=timeout_seconds,
        timeout_seconds_path=timeout_seconds_path,
        heartbeat_seconds=heartbeat_seconds,
        heartbeat_seconds_path=heartbeat_seconds_path,
        comment=comment,
        input_path=input_path,
        parameters=parameters,
        result_path=result_path,
        output_path=output_path,
        ...)
      if (wait_for_completion)
        kwargs[Field$Resource] = get_service_integration_arn(
          EKS_SERVICE_NAME,
          EksApi$CreateNodegroup,
          IntegrationPattern$WaitForCompletion)
      else
        kwargs[[Field$Resource]] = get_service_integration_arn(
          EKS_SERVICE_NAME,
          EksApi$CreateNodegroup)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EksDeleteNodegroupStep class
#' @description Creates a Task State to delete node groups for a `AWS EKS cluster`.
#'              See `Manage Amazon EKS` with Step Functions
#'              \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-eks.html}
#'              for more details.
#' @export
EksDeleteNodegroupStep = R6Class("EksDeleteNodegroupStep",
  inherit = Task,
  public = list(

    #' @description Initialize EksDeleteNodegroupStep Task class
    #' @param state_id (str): State name whose length **must be** less than or
    #'              equal to 128 unicode characters. State names **must be** unique
    #'              within the scope of the whole state machine.
    #' @param wait_for_completion (bool, optional): Boolean value set to `True` if
    #'              the Task state should wait for the glue job to complete before proceeding
    #'              to the next step in the workflow. Set to `False` if the Task state should
    #'              submit the glue job and proceed to the next step. (default: True)
    #' @param timeout_seconds (int, optional): Positive integer specifying timeout for the
    #'              state in seconds. If the state runs longer than the specified timeout,
    #'              then the interpreter fails the state with a `States.Timeout` Error Name. (default: 60)
    #' @param timeout_seconds_path (str, optional): Path specifying the state's timeout
    #'              value in seconds from the state input. When resolved, the path must select
    #'              a field whose value is a positive integer.
    #' @param heartbeat_seconds (int, optional): Positive integer specifying heartbeat
    #'              timeout for the state in seconds. This value should be lower than
    #'              the one specified for `timeout_seconds`. If more time than the specified
    #'              heartbeat elapses between heartbeats from the task, then the interpreter
    #'              fails the state with a `States.Timeout` Error Name.
    #' @param heartbeat_seconds_path (str, optional): Path specifying the state's
    #'              heartbeat value in seconds from the state input. When resolved,
    #'              the path must select a field whose value is a positive integer.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          wait_for_completion=TRUE,
                          timeout_seconds=NULL,
                          timeout_seconds_path=NULL,
                          heartbeat_seconds=NULL,
                          heartbeat_seconds_path=NULL,
                          comment=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = list(
        state_id=state_id,
        timeout_seconds=timeout_seconds,
        timeout_seconds_path=timeout_seconds_path,
        heartbeat_seconds=heartbeat_seconds,
        heartbeat_seconds_path=heartbeat_seconds_path,
        comment=comment,
        input_path=input_path,
        parameters=parameters,
        result_path=result_path,
        output_path=output_path,
        ...)
      if (wait_for_completion)
        kwargs[Field$Resource] = get_service_integration_arn(
        EKS_SERVICE_NAME,
        EksApi$DeleteNodegroup,
        IntegrationPattern$WaitForCompletion)
      else
        kwargs[[Field$Resource]] = get_service_integration_arn(
        EKS_SERVICE_NAME,
        EksApi$DeleteNodegroup)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)
