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
                          comment=NULL,
                          wait_for_callback=FALSE,
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
                         comment=NULL,
                         wait_for_callback=FALSE,
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
                         comment=NULL,
                         wait_for_completion=TRUE,
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
                         comment=NULL,
                         wait_for_completion=TRUE,
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
          lasticMapReduceApi$TerminateCluster)

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
                          comment=NULL,
                          wait_for_completion=TRUE,
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

#' @title EmrModifyInstanceFleetByNameStep class
#' @description Creates a Task state to modify the number of nodes and configuration
#'              settings of an instance group. See Call Amazon EMR with Step Functions
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
      kwargs[[Field$Resource]] = et_service_integration_arn(
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
                          comment=NULL,
                          wait_for_completion=TRUE,
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

      kwargs[[Field$Resource]] = get_service_integration_arn(
        CODEBUILD_SERVICE_NAME,
        CodeBuildApi$stopBuild)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title CodeBuildBatchDeleteBuildsStep class
#' @description Creates a Task state to stop code build jobs. See Call Amazon Code Build
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

      kwargs[[Field$Resource]] = get_service_integration_arn(
        CODEBUILD_SERVICE_NAME,
        CodeBuildApi$BatchDeleteBuilds)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title CodeBuildBatchGetReportsStep class
#' @description Creates a Task state to stop code build jobs. See Call Amazon Code Build
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

      kwargs[[Field$Resource]] = get_service_integration_arn(
        CODEBUILD_SERVICE_NAME,
        CodeBuildApi$BatchGetReports)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)




