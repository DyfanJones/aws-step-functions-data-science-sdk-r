# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/src/stepfunctions/steps/compute.py

#' @include steps_fields.R
#' @include steps_states.R
#' @include steps_integrate_resources.R
#' @include utils.R

#' @import R6

LAMBDA_SERVICE_NAME = "lambda"
GLUE_SERVICE_NAME = "glue"
ECS_SERVICE_NAME = "ecs"
BATCH_SERVICE_NAME = "batch"

LambdaApi = Enum(
  Invoke = "invoke"
)

GlueApi = Enum(
  StartJobRun = "startJobRun"
)

EcsApi = Enum(
  RunTask = "runTask"
)

BatchApi = Enum(
  SubmitJob = "submitJob"
)

#' @title LambdaStep class
#' @description Creates a Task state to invoke an AWS Lambda function.
#'              See Invoke Lambda with Step Functions
#'              \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-lambda.html}
#'              for more details.
#' @export
LambdaStep = R6Class("LambdaStep",
  inherit = Task,
  public = list(

    #' @description Initialize LambdaStep Task class
    #' @param state_id (str): State name whose length **must be** less than or
    #'              equal to 128 unicode characters. State names **must be** unique
    #'              within the scope of the whole state machine.
    #' @param wait_for_callback (bool, optional): Boolean value set to `True` if the
    #'              Task state should wait for callback to resume the operation. (default: False)
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
                          wait_for_callback=FALSE,
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

      if (wait_for_callback)
        kwargs[[Field$Resource]] = get_service_integration_arn(
          LAMBDA_SERVICE_NAME,
          LambdaApi$Invoke,
          IntegrationPattern$WaitForTaskToken)
      else
        kwargs[[Field$Resource]] = get_service_integration_arn(
          LAMBDA_SERVICE_NAME, LambdaApi$Invoke)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title GlueStartJobRunStep Class
#' @description Creates a Task state to run an AWS Glue job. See `Manage AWS Glue Jobs`
#'              `with Step Functions <https://docs.aws.amazon.com/step-functions/latest/dg/connect-glue.html>`_
#'              for more details.
#' @export
GlueStartJobRunStep = R6Class("GlueStartJobRunStep",
  inherit = Task,
  public = list(

    #' @description Initialize LambdaStep Task class
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
        kwargs[[Field$Resource]] = get_service_integration_arn(
          GLUE_SERVICE_NAME,
          GlueApi$StartJobRun,
          IntegrationPattern$WaitForCompletion)
      else
        kwargs[[Field$Resource]] = get_service_integration_arn(
          GLUE_SERVICE_NAME,
          GlueApi$StartJobRun)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title BatchSubmitJobStep Class
#' @description Creates a Task State to start an AWS Batch job. See `Manage AWS Batch`
#'              with Step Functions \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-batch.html}
#'              for more details.
#' @export
BatchSubmitJobStep = R6Class("BatchSubmitJobStep",
  inherit = Task,
  public = list(

    #' @description Initialize LambdaStep Task class
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
        kwargs[[Field$Resource]] = get_service_integration_arn(
          BATCH_SERVICE_NAME,
          BatchApi$SubmitJob,
          IntegrationPattern$WaitForCompletion)
      else
        kwargs[[Field$Resource]] = get_service_integration_arn(
          BATCH_SERVICE_NAME,
          BatchApi$SubmitJob)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title EcsRunTaskStep class
#' @description Creates a Task State to run Amazon ECS or Fargate Tasks. See `Manage Amazon ECS`
#'              or Fargate Tasks with Step Functions
#'              \url{https://docs.aws.amazon.com/step-functions/latest/dg/connect-ecs.html}
#'              for more details.
#' @export
EcsRunTaskStep = R6Class("EcsRunTaskStep",
  inherit = Task,
  public = list(

    #' @description Initialize LambdaStep Task class
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
        kwargs[Field$Resource] = et_service_integration_arn(
          ECS_SERVICE_NAME,
          EcsApi$RunTask,
          IntegrationPattern$WaitForCompletion)
      else
        kwargs[[Field$Resource]] = get_service_integration_arn(
          ECS_SERVICE_NAME,
          EcsApi$RunTask)

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)
