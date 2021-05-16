# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/src/stepfunctions/workflow/widgets/events_table.py

#' @import R6
#' @import jsonlite
#' @importFrom glue glue

#' @include workflow_widgets_utils.R
#' @include utils.R

LAMBDA_SERVICE_NAME = "lambda"
LAMBDA_FUNCTION_RESOURCE_TYPE = "function"
LAMBDA_ARN_SEGMENT_LENGTH = 7
SAGEMAKER_JOB_NAME_MAP = list(
  'createTrainingJob'='Sagemaker training job',
  'createTrainingJob.sync'='Sagemaker training job',
  'createTransformJob'='Sagemaker transform job',
  'createTransformJob.sync'='Sagemaker transform job',
  'createModel'='Sagemaker model',
  'createModel.sync'='Sagemaker model',
  'createEndpointConfig'='Sagemaker endpoint configuration',
  'createEndpointConfig.sync'='Sagemaker endpoint configuration',
  'createEndpoint'='Sagemaker endpoint',
  'createEndpoint.sync'='Sagemaker endpoint'
)

EVENT_TABLE_TEMPLATE = '
    <style>
        $$aws_table_css$$
        $$custom_css$$
    </style>
    <table class="table-widget">
        <thead>
            <tr>
                <th style="width: 60px">ID</th>
                <th>Type</th>
                <th>Step</th>
                <th>Resource</th>
                <th>Elapsed Time (ms)</th>
                <th>Timestamp</th>
            </tr>
        </thead>
        <tbody>
            {table_rows}
        </tbody>
    </table>
    <script type="text/javascript">
        $$js$$
    </script>
'

EVENT_TABLE_ROW_TEMPLATE = '
    <tr class="awsui-table-row">
        <td class="awsui-util-pl-xs clickable-cell">
            <div class="toggle-icon"></div>
            <span>$$event_id$$</span>
        </td>
        <td>$$event_type$$</td>
        <td>$$step$$</td>
        <td><a $$resource_url$$ target="_blank">$$resource$$</a></td>
        <td>$$elapsed_time$$</td>
        <td>$$timestamp$$</td>
    </tr>
    <tr class="hide">
        <td class="execution-event-detail" colspan="6">
            <pre>$$event_detail$$</pre>
        </td>
    </tr>
'

EVENT_JS_TEMPLATE = '
    var clickableCells = document.getElementsByClassName("clickable-cell");
    for (var cell of clickableCells) {
        cell.addEventListener("click", function(e) {
            var currentRow = e.srcElement.closest("tr");
            var toggleRow = currentRow.nextElementSibling;
            var toggleArrow = currentRow.getElementsByClassName("toggle-icon")[0];
            toggleRow.classList.toggle("hide");
            toggleArrow.classList.toggle("open");
        });
    }
'

EVENT_CSS_TEMPLATE = '
    .table-widget .clickable-cell {
        padding-left: 0.1em;
        cursor: pointer;
    }
    .toggle-icon {
        display: inline-block;
        width: 0;
        height: 0;
        border-top: 5px solid transparent;
        border-left: 8px solid #545b64;
        border-bottom: 5px solid transparent;
        margin-right: 5px;
    }
    .toggle-icon.open {
        -webkit-transform: rotate(90deg);
        -ms-transform: rotate(90deg);
        transform: rotate(90deg);
    }
'

EventsTableWidget = R6Class("EventsTableWidget",
  public = list(
    initialize = function(events){
      self$eventIdToLambdaArnMap = list()
      self$previous_step_name = ""
      self$previous_job_name = ""
      start_datetime = NULL

      if (length(events) > 0)
        start_datetime = events[[1]][["timestamp"]]

      table_rows = lapply(events, function(event){
        glue(EVENT_TABLE_ROW_TEMPLATE,
             event_id=as.character(event[["id"]]),
             event_type=event[["type"]],
             step=private$.get_step(event),
             resource=private$.get_resource(event, True),
             resource_url=private$.get_resource_url(event),
             elapsed_time=get_elapsed_ms(start_datetime, event[["timestamp"]]),
             timestamp=format_time(event[["timestamp"]]),
             event_detail=private$.format_event_detail(event),
             .open = "$$",
             .close = "$$")
      })
      self$template = glue(EVENT_TABLE_TEMPLATE,table_rows=paste(table_rows, collapse = '\n'))
    },

    show = function(){
      return(glue(self$template,
        aws_table_css=AWS_TABLE_CSS,
        custom_css=EVENT_CSS_TEMPLATE,
        js=EVENT_JS_TEMPLATE,
        .open = "$$",
        .close = "$$")
      )
    }
  ),

  private = list(
    .get_step_detail = function(event){
      switcher = switch(names(event[["type"]]) %||% NA,
            "ChoiceStateEntered"=event[["stateEnteredEventDetails"]],
            "ChoiceStateExited"=event[["stateExitedEventDetails"]],
            "FailStateEntered"=event[["stateEnteredEventDetails"]],
            "MapStateEntered"=event[["stateEnteredEventDetails"]],
            "MapStateExited"=event[["stateExitedEventDetails"]],
            "ParallelStateEntered"=event[["stateEnteredEventDetails"]],
            "ParallelStateExited"=event[["stateExitedEventDetails"]],
            "PassStateEntered"=event[["stateEnteredEventDetails"]],
            "PassStateExited"=event[["stateExitedEventDetails"]],
            "SucceedStateEntered"=event[["stateEnteredEventDetails"]],
            "SucceedStateExited"=event[["stateExitedEventDetails"]],
            "TaskStateEntered"=event[["stateEnteredEventDetails"]],
            "TaskStateExited"=event[["stateExitedEventDetails"]],
            "WaitStateEntered"=event[["stateEnteredEventDetails"]],
            "WaitStateExited"=event[["stateExitedEventDetails"]],
            "MapIterationAborted"=event[["mapIterationAbortedEventDetails"]],
            "MapIterationFailed"=event[["mapIterationFailedEventDetails"]],
            "MapIterationStarted"=event[["mapIterationStartedEventDetails"]],
            "MapIterationSucceeded"=event[["mapIterationSucceededEventDetails"]],
            "ExecutionFailed"=event[["executionFailedEventDetails"]],
            "ExecutionStarted"=event[["executionStartedEventDetails"]],
            "ExecutionSucceeded"=event[["executionSucceededEventDetails"]],
            "ExecutionAborted"=event[["executionAbortedEventDetails"]],
            "ExecutionTimedOut"=event[["executionTimedOutEventDetails"]],
            "LambdaFunctionScheduled"=event[["lambdaFunctionScheduledEventDetails"]],
            "LambdaFunctionScheduleFailed"=event[["lambdaFunctionScheduleFailedEventDetails"]],
            "LambdaFunctionStartFailed"=event[["lambdaFunctionStartFailedEventDetails"]],
            "LambdaFunctionSucceeded"=event[["lambdaFunctionSucceededEventDetails"]],
            "LambdaFunctionFailed"=event[["lambdaFunctionFailedEventDetails"]],
            "LambdaFunctionTimedOut"=event[["lambdaFunctionTimedOutEventDetails"]],
            "TaskStarted"=event[["taskStartedEventDetails"]],
            "TaskSubmitted"=event[["taskSubmittedEventDetails"]],
            "TaskScheduled"=event[["taskScheduledEventDetails"]],
            "TaskSucceeded"=event[["taskSucceededEventDetails"]],
            "TaskFailed"=event[["taskFailedEventDetails"]])
      return(switcher %||% list())
    },

    # Tries to get step name, if it can not find, return the previous step's name
    .get_step = function(event){
      if (names(event[["type"]]) %in% c(
        "ExecutionFailed",
        "ExecutionStarted",
        "ExecutionSucceeded",
        "ExecutionAborted",
        "ExecutionTimedOut")){
        step_name = ""
        self$previous_step_name = ""
      } else {
        step_name = private$.get_step_detail(event)[["name"]]
        if (is.null(step_name))
          step_name = self$previous_step_name
        else
          self$previous_step_name = step_name
      }
      return(step_name)
    },

    .get_resource = function(event, mapped_value=FALSE){
      # check that it is a lambda, sagemaker or just a regular execution
      if (private$.is_correct_lambda_arn_sequence(private$.get_lambda_arn(event))){
        return("Lambda")

      # check if it has a resource
      } else if (private$.has_resource(event)){
        # check if it is a sagemaker resource
        step_details = private$.get_step_detail(event)
        if (step_details[["resourceType"]] == "sagemaker"){
          sagemaker_resource = step_details[["resource"]]

          if (mapped_value)
            return(SAGEMAKER_JOB_NAME_MAP[sagemaker_resource])

          return(sagemaker_resource)
        }
        return("Step Functions execution")
      }

      # if not a resource, return -
      return("-")
    },

    .get_resource_url = function(event){
      resource = private$.get_resource(event)

      if ("createTrainingJob" %in% names(resource)){
        job_name = private$.get_sagemaker_resource_job_name(event, "TrainingJobName")
        return(sprintf('href="%s"',sagemaker_console_link('jobs', job_name)))
      }
      if ("createTransformJob" %in% names(resource)){
          job_name = private$.get_sagemaker_resource_job_name(event, "TransformJobName")
        return(sprintf('href="%s"',sagemaker_console_link('transformJobs', job_name)))
      }
      if ("createModel" %in% names(resource)){
        job_name = private$.get_sagemaker_resource_job_name(event, "ModelName")
        return(sprintf('href="%s"', sagemaker_console_link('models', job_name)))
      }
      if ("createEndpointConfig" %in% names(resource)){
        job_name = private$.get_sagemaker_resource_job_name(event, "EndpointConfigName")
        return(sprintf('href="%s"',sagemaker_console_link('endpointConfig', job_name)))
      }
      if ("createEndpoint" %in% names(resource)){
        job_name = private$.get_sagemaker_resource_job_name(event, "EndpointName")
        return(sprintf('href="%s"',sagemaker_console_link('endpoints', job_name)))
      }

      self$previous_job_name = ""
      return("class='disabled'")
    },

    .get_sagemaker_resource_job_name = function(event, job_name_key){
      step_details = private$.get_step_detail(event)
      job_name = step_details[["parameters"]][[job_name_key]] %||% ""
      if (job_name == "")
        job_name = self$previous_job_name
      else
        self$previous_job_name = job_name

      return(job_name)
    },

    .has_resource = function(event){
      return(any(names(event[["type"]]) %in% c(
        "TaskSucceeded",
        "TaskSubmitted",
        "TaskScheduled",
        "TaskStarted"
      )))
    },

    .get_lambda_arn = function(event){
      resource_arn = "-"
      event_type = event[["type"]]

      if (event_type == "LambdaFunctionScheduled"){
        resource_arn = event[["lambdaFunctionScheduledEventDetails"]][["resource"]]
      } else if (event_type %in% c(
        "LambdaFunctionScheduleFailed",
        "LambdaFunctionFailed",
        "LambdaFunctionStartFailed",
        "LambdaFunctionStarted",
        "LambdaFunctionSucceeded",
        "LambdaFunctionTimedOut")){
        resource_arn = self$eventIdToLambdaArnMap[[event[["previousEventId"]]]]
      }
      self$eventIdToLambdaArnMap[[event[["id"]]]] = resource_arn
      return(resource_arn)
    },

    .is_correct_lambda_arn_sequence = function(lambda_arn){
      lambda_arn_segments = unlist(strsplit(lambda_arn, split=":"))
      return (length(lambda_arn_segments) == LAMBDA_ARN_SEGMENT_LENGTH &&
              lambda_arn_segments[3] == LAMBDA_SERVICE_NAME &&
              lambda_arn_segments[6] == LAMBDA_FUNCTION_RESOURCE_TYPE)
    },

    .format_event_detail = function(event){
      event_details = private$.get_step_detail(event)
      private$.unpack_to_proper_dict(event_details)
      return(toJSON(event_details, pretty=T, auto_unbox=T))
    },

    .unpack_to_proper_dict = function(dictionary){
      for (k in dictionary){
        v = dictionary[[k]]
        if (inherits(v, "list"))
          private$.unpack_to_proper_dict(v)
        else
          dictionary[[k]] = private$.load_json(v)
      }
    },

    .load_json = function(value){
      tryCatch({
        return(fromJSON(value))
      },
      error = function(e){
        value
      })
    }

  ),
  lock_objects=F
)
