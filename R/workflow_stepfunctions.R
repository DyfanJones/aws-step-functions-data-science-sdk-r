# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/src/stepfunctions/workflow/stepfunctions.py

#' @importFrom paws sfn
#' @import R6
#' @import jsonlite
#' @import lgr

#' @include utils.R
#' @include steps_states.R
#' @include workflow_widgets_graph.R
#' @include workflow_widgets_event_table.R
#' @include workflow_widgets_workflows_table.R
#' @include workflow_widgets_utils.R
#' @include workflow_cloudformation.R


EventsList = R6Class("EventsList",
  inherit = EventsTableWidget,
  public = list(
    to_html = function(){
      return(self$show())
    }
  ),
  lock_objects=F
)

WorkflowList = R6Class("WorkflowList",
  inherit = WorkflowsTableWidget,
  public = list(
    to_html = function(){
      return(self$show())
    }
  ),
  lock_objects=F
)

ExecutionsList = R6Class("ExecutionsList",
  inherit = ExecutionsTableWidget,
  public = list(
    to_html = function(){
      return(self$show())
    }
  ),
  lock_objects=F
)

ExecutionStatus = Enum(
  Running   = 'RUNNING',
  Succeeded = 'SUCCEEDED',
  Failed    = 'FAILED',
  TimedOut  = 'TIMED_OUT',
  Aborted   = 'ABORTED'
)


#' @title Workflow Class
#' @description Class for creating and managing a workflow.
#' @export
Workflow = R6Class("Workflow",
  public = list(

    #' @description Lists all the workflows in the account.
    #' @param max_items (int, optional): The maximum number of items to be returned. (default: 100)
    #' @param client (SFN.Client, optional): boto3 client to use for the query. If
    #'              not provided, a default boto3 client for Step Functions will be
    #'              automatically created and used. (default: None)
    #' @param html (bool, optional): Renders the list as an HTML table (If running in
    #'              an IPython environment). If the parameter is not provided, or set
    #'              to False, a Python list is returned. (default: False)
    #' @return list: The list of workflows. Refer to :meth:`.SFN.Client.list_state_machines()`
    #'              for the response structure.
    list_workflows = function(max_items=100,
                              client=NULL,
                              html=FALSE){
      if (is.null(client)){
        LOGGER$debug(paste("The argument 'client' is not provided. Creating a new",
                           "boto3 client instance with default settings."))
        client = sfn()
      }

      LOGGER$debug("Retrieving list of workflows from AWS Step Functions.")

      token = NULL
      workflows = list()
      while(!identical(token, character(0))){
        batch_response=client$list_state_machines(
          maxResults=max_items,
          nextToken=token
        )

        workflows = c(workflows, batch_response[["stateMachines"]])
        token=batch_response[["nextToken"]]
      }

      if (html){
        display_html <- pkg_method("display_html","IRdisplay")
        return(display_html(WorkflowList$new(workflows)$to_html()))
      } else {
        return(workflows)
      }
    },

    #' @description Initialize Workflow Class
    #' @param name (str): The name of the workflow. A name must not contain:
    #'              - whitespace
    #'              - brackets `< > { } [ ]`
    #'              - wildcard characters `? *`
    #'              - special characters `` \" # % \ ^ | ~ \` $ & , ; : / ``
    #'              - control characters (`U+0000-001F`, `U+007F-009F`)
    #' @param definition (State or Chain): The `Amazon States Language`
    #'              `<https://states-language.net/spec.html>`_ definition of the workflow.
    #' @param role (str): The Amazon Resource Name (ARN) of the IAM role to use for creating,
    #'              managing, and running the workflow.
    #' @param tags (list): Tags to be added when creating a workflow. Tags are key-value pairs
    #'              that can be associated with Step Functions workflows and activities. (default: [])
    #' @param execution_input (ExecutionInput, optional): Placeholder collection that defines
    #'              the placeholder variables for the workflow execution. This is also used
    #'              to validate inputs provided when executing the workflow. (default: None)
    #' @param timeout_seconds (int, optional): The maximum number of seconds an execution
    #'              of the workflow can run. If it runs longer than the specified time,
    #'              the workflow run fails with a `States.Timeout` Error Name. (default: None)
    #' @param comment (str, optional): A human-readable description of the workflow. (default: None)
    #' @param version (str, optional): The version of the Amazon States Language used
    #'              in the workflow. (default: None)
    #' @param state_machine_arn (str, optional): The Amazon Resource Name (ARN) of
    #'              the workflow. (default: None)
    #' @param format_json (bool, optional): Boolean flag set to `True` if workflow
    #'              definition and execution inputs should be prettified for this workflow.
    #'              `False`, otherwise. (default: True)
    #' @param client (SFN.Client, optional): boto3 client to use for creating, managing,
    #'              and running the workflow on Step Functions. If not provided, a
    #'              default boto3 client for Step Functions will be automatically created
    #'              and used. (default: None)
    initialize = function(name,
                          definition,
                          role,
                          tags=list(),
                          execution_input=NULL,
                          timeout_seconds=NULL,
                          comment=NULL,
                          version=NULL,
                          state_machine_arn=NULL,
                          format_json=TRUE,
                          client=NULL){
      self$timeout_seconds = timeout_seconds
      self$comment = comment
      self$version = version
      if (inherits(definition, "Graph"))
        self$definition = definition
      else
        self$definition = Graph$new(
          definition,
          timeout_seconds=self$timeout_seconds,
          comment=self$comment,
          version=self$version
        )
      self$name = name
      self$role = role
      self$tags = tags
      self$workflow_input = execution_input

      if (!is.null(client))
        self$client = client
      else
        self$client = sfn()

      self$format_json = format_json
      self$state_machine_arn = state_machine_arn
    },

    #' @description Factory method to create an instance attached to an exisiting
    #'              workflow in Step Functions.
    #' @param state_machine_arn (str): The Amazon Resource Name (ARN) of the existing workflow.
    #' @param client (SFN.Client, optional): boto3 client to use for attaching the existing
    #'              workflow in Step Functions to the Workflow object. If not provided,
    #'              a default boto3 client for Step Functions will be automatically
    #'              created and used. (default: None)
    #' @return Workflow: Workflow object attached to the existing workflow in Step Functions.
    attach = function(state_machine_arn,
                      client=NULL){

      if (!is.null(client)){
        LOGGER$debug(paste("The argument 'client' is not provided. Creating a new",
                    "boto3 client instance with default settings."))
        client = sfn()
      }
      response = client$describe_state_machine(stateMachineArn=state_machine_arn)
      return(Workflow$new(
        name=response[['name']],
        definition=FrozenGraph$public_methods$from_json(response[['definition']]),
        role=response[['roleArn']],
        state_machine_arn=response[['stateMachineArn']],
        client=client)
      )
    },

    #' @description Creates the workflow on Step Functions.
    #' @return str: The Amazon Resource Name (ARN) of the workflow created. If the workflow
    #'              already existed, the ARN of the existing workflow is returned.
    create = function(){
      if (!is.null(self$state_machine_arn)){
        LOGGER$warning("The workflow already exists on AWS Step Functions. No action will be performed.")
        return(self$state_machine_arn)
      }

      tryCatch({
        self$state_machine_arn = private$.create()
      },
      error = function(e){
        self$state_machine_arn = private$.extract_state_machine_arn(e)
        LOGGER$error(paste("A workflow with the same name already exists on AWS",
        "Step Functions. To update a workflow, use Workflow.update()."))
      })
      return(self$state_machine_arn)
    },

    #' @description Updates an existing state machine by modifying its definition
    #'              and/or role. Executions started immediately after calling this
    #'              method may use the previous definition and role.
    #' @param definition (State or Chain, optional): The `Amazon States Language`
    #'              `<https://states-language.net/spec.html>`_ definition to update
    #'              the workflow with. (default: None)
    #' @param role (str, optional): The Amazon Resource Name (ARN) of the IAM role
    #'              to use for creating, managing, and running the workflow. (default: None)
    #' @return str: The state machine definition and/or role updated. If the update fails,
    #' None will be returned.
    update = function(definition=NULL,
                      role=NULL){
      if (is.null(definition) && is.null(role))
        stop("A new definition and/or role must be provided to update an existing workflow.")

      if (is.null(self$state_machine_arn))
        stop("Local workflow instance does not point to an existing workflow ",
        "on AWS StepFunctions. Please consider using Workflow.create(...) ",
        "to create a new workflow, or Workflow.attach(...) to attach the ",
        "instance to an existing workflow on AWS Step Functions.")

      if (!is.null(definition)){
        if (inherits(definition, "Graph"))
          self$definition = definition
        else
          self$definition = Graph$new(
            definition,
            timeout_seconds=self$timeout_seconds,
            comment=self$comment,
            version=self$version)
      }
      if(!is.null(role))
        self$role = role

      response = self$client$update_state_machine(
        stateMachineArn=self$state_machine_arn,
        definition=self$definition$to_json(pretty=self$format_json),
        roleArn=self$role
      )
      LOGGER$info(paste("Workflow updated successfully on AWS Step Functions.",
      "All execute() calls will use the updated definition and role within a few seconds."))
      return(self$state_machine_arn)
    },

    #' @description Starts a single execution of the workflow.
    #' @param name (str, optional): The name of the workflow execution. If one is
    #'              not provided, a workflow execution name will be auto-generated. (default: None)
    #' @param inputs (str, list or dict, optional): Input data for the workflow execution. (default: None)
    #' @return stepfunctions.workflow.Execution: An execution instance of the workflow.
    execute = function(name=NULL, inputs=NULL){
      if (!is.null(self$workflow_input))
        validation_result = self$workflow_input$validate(inputs)
      if (isFALSE(validation_result$valid))
        stop(sprintf(
          "Expected run input with the schema: %s",
          self$workflow_input$get_schema_as_json()))

      if (is.null(self$state_machine_arn))
        stop("Local workflow instance does not point to an existing workflow on ",
             "AWS StepFunctions. Before executing a workflow, call Workflow.create(...) ",
             "or Workflow.attach(...).")

      params = list(
        'stateMachineArn'=self$state_machine_arn)
      params[['name']] = name

      if (!is.null(inputs))
        params[['input']] = toJSON(inputs, pretty=self$format_json, auto_unbox=T)

      response = self$client$start_execution(
        stateMachineArn=params$stateMachineArn,
        name=params$name,
        input=params$input)

      LOGGER$info("Workflow execution started successfully on AWS Step Functions.")

      # name is None because boto3 client.start_execution only returns startDate and executionArn
      return(Execution$new(
        workflow=self,
        execution_arn=response[['executionArn']],
        start_date=response[['startDate']],
        status=ExecutionStatus$Running,
        client=self$client)
      )
    },

    #' @description Lists the executions for the workflow.
    #' @param max_items (int, optional): The maximum number of items to be returned. (default: 100)
    #' @param status_filter (ExecutionStatus, optional): If specified, only list
    #'              the executions whose current status matches the given filter. (default: None)
    #' @param html (bool, optional): Renders the list as an HTML table (If running
    #'              in an IPython environment). If the parameter is not provided, or
    #'              set to False, a Python list is returned. (default: False)
    #' @return list: List of workflow run instances.
    list_executions = function(max_items=100,
                               status_filter=NULL,
                               html=FALSE){
      if (is.null(self$state_machine_arn))
        return(list())

      LOGGER$debug("Retrieving list of executions from AWS Step Functions.")

      token=NULL
      response = list()

      while(!identical(token, character(0))){
        batch_response=response=svc$list_executions(
          stateMachineArn=self$state_machine_arn,
          statusFilter=if(is.null(status_filter)) status_filter else toupper(status_filter),
          maxResults=max_items,
          nextToken=token)

        response = c(response, batch_response["executions"])
        token = batch_response[["nextToken"]]
      }
      runs = lapply(response, function(execution){
        Execution$new(
          name=execution[['name']],
          workflow=self,
          execution_arn=execution[['executionArn']],
          start_date=execution[['startDate']],
          stop_date=execution[['stopDate']],
          status=execution[['status']],
          client=self$client
        )
      })

      if (html){
        display_html <- pkg_method("display_html","IRdisplay")
        return(display_html(ExecutionsList$new(runs)$to_html()))
      } else {
        return(runs)
      }
    },

    #' @description  Deletes the workflow, if it exists.
    delete = function(){
      if (!is.null(self$state_machine_arn))
        self$client$delete_state_machine(stateMachineArn=self$state_machine_arn)
      LOGGER$info(paste("Workflow has been marked for deletion. If the workflow has",
      "running executions, it will be deleted when all executions are stopped."))
    },

    #' @description Renders a visualization of the workflow graph.
    #' @param portrait (bool, optional): Boolean flag set to `True` if the workflow
    #'              graph should be rendered in portrait orientation. Set to `False`,
    #'              if the graph should be rendered in landscape orientation. (default: False)
    render_graph = function(portrait = FALSE){
      widget = WorkflowGraphWidget$new(self$definition$to_json())
      return(widget$show(portrait=portrait))
    },

    #' @description Returns a CloudFormation template that contains only the StateMachine
    #'              resource. To reuse the CloudFormation template in a different region,
    #'              please make sure to update the region specific AWS resources (e.g: Lambda
    #'              ARN, Training Image) in the StateMachine definition.
    get_cloudformation_template = function(){
      return(build_cloudformation_template(self))
    },

    #' @description class formatting
    format = function(){
      # identify if in Rstudio or not
      if (pkg_env$jupyter){
        if (!is.null(self$state_machine_arn))
          return(sprintf(
            'Workflow: <a target="_blank" href="%s">%s</a>',
            create_sfn_workflow_url(self$state_machine_arn), self$state_machine_arn))
        else
          return('Workflow: Does Not Exist.')
      } else {
        if (!is.null(self$state_machine_arn)){
          cls_fmt = "%s(name='%s', role='%s', state_machine_arn='%s')"
          return(sprintf(cls_fmt, class(self)[1],
                 self$name, self$role, self$state_machine_arn))
        } else {
          cls_fmt = "%s(name='%s', role='%s'): Does Not Exist"
          return(sprintf(cls_fmt, class(self)[1],
                 self$name, self$role))
        }
      }
    },

    #' @description print class
    print = function(){
      if(pkg_env$jupyter){
        display_html <- pkg_method("display_html","IRdisplay")
        display_html(self$format())
      } else {
        cat(self$format(), "\n")
      }
    }
  ),
  private = list(
    .create = function(){
      response = self$client$create_state_machine(
        name=self$name,
        definition=self$definition$to_json(pretty=self$format_json),
        roleArn=self$role,
        tags=self$tags)
      LOGGER$info("Workflow created successfully on AWS Step Functions.")
      return(response[['stateMachineArn']])
    },

    .extract_state_machine_arn = function(exception){
      message = attributes(e)$error_response$message
      return(unlist(strsplit(message, "'"))[2])
    }
  ),
  lock_objects=F
)

#' @title Execution class
#' @description Class for managing a workflow execution.
#' @export
Execution = R6Class("Execution",
  public = list(

    #' @description Initialize Execution class
    #' @param workflow (Workflow): Step Functions workflow instance.
    #' @param execution_arn (str): The Amazon Resource Name (ARN) of the workflow execution.
    #' @param start_date (datetime.datetime): The date the workflow execution was started.
    #' @param status (RunStatus): Status of the workflow execution.
    #' @param client (SFN.Client, optional): boto3 client to use for running and
    #'              managing the workflow executions on Step Functions. If no client
    #'              is provided, the boto3 client from the parent workflow will be used. (default: None)
    #' @param name (str, optional): Name for the workflow execution. (default: None)
    #' @param stop_date (datetime.datetime, optional): The date the workflow execution
    #'              was stopped, if applicable. (default: None)
    inherit = function(workflow,
                       execution_arn,
                       start_date,
                       status,
                       client=NULL,
                       name=NULL,
                       stop_date=NULL){
      self$name = name
      self$workflow = workflow
      self$execution_arn = execution_arn
      self$start_date = start_date
      self$stop_date = stop_date
      self$status = status
      if (!is.null(client))
        self$client = client
      else
        self$client = self$workflow$client
    },

    #' @description Stops a workflow execution.
    #' @param error (str, optional): The error code of the failure. (default: None)
    #' @param cause (str, optional): A more detailed explanation of the cause of the failure. (default: None)
    #' @return dict: Datetime of when the workflow execution was stopped. Example below::
    #' ```
    #' list(
    #'  stopDate = as.POSIXct(
    #'    "2015-01-01"
    #'  )
    #' )
    #' ```
    #' **Response structure**:
    #'  - (dict)
    #'  - stopDate (datetime): The date the workflow execution is stopped
    stop = function(cause=NULL,
                    error=NULL){
      response = self$client$stop_execution(
        executionArn=self$execution_arn,
        cause=cause,
        error=error
      )
      return(response)
    },

    #' @description Lists the events in the workflow execution.
    #' @param max_items (int, optional): The maximum number of items to be returned.
    #'              (default: 100)
    #' @param reverse_order (bool, optional): Boolean flag set to `True` if the events
    #'              should be listed in reverse chronological order. Set to `False`,
    #'              if the order should be in chronological order. (default: False)
    #' @param html (bool, optional): Renders the list as an HTML table (If running in
    #'              an IPython environment). If the parameter is not provided, or set
    #'              to False, a Python list is returned. (default: False)
    #' @return dict: Object containing the list of workflow execution events. Refer
    #'              to :meth:`.SFN.Client.get_execution_history()` for the response structure.
    list_events = function(max_items=100,
                           reverse_order=FALSE,
                           html=FALSE){
      LOGGER$debug("Retrieving list of history events for your execution from AWS Step Functions.")

      events=list()
      token=NULL
      while(identical){
        batch_response = self$client$get_execution_history(
          executionArn=self$execution_arn,
          maxResults=max_items,
          reverseOrder=executionArn,
          nextToken=token
        )
        events=c(events, batch_response[["events"]])
        token=batch_response[["nextToken"]]
      }

      if (html){
        display_html <- pkg_method("display_html","IRdisplay")
        return(display_html(EventsList$new(events)$to_html()))
      } else {
        return(events)
      }
    },

    #' @description Describes a workflow execution.
    #' @return dict: Details of the workflow execution.
    #' **Response structure**:
    #' - executionArn (string): The Amazon Resource Name (ARN) that identifies the workflow execution.
    #' - stateMachineArn (string): The Amazon Resource Name (ARN) of the workflow that was executed.
    #' - name (string): The name of the workflow execution.
    #' - status (string): The current status of the workflow execution.
    #' - startDate (datetime): The date the workflow execution is started.
    #' - stopDate (datetime): If the workflow execution has already ended, the date the execution stopped.
    #' - input (string): The string that contains the JSON input data of the workflow execution.
    #' - output (string): The JSON output data of the workflow execution.
    describe = function(){
      return(self$client$describe_execution(executionArn=self$execution_arn))
    },

    #' @description Renders a visualization of the workflow execution graph.
    #' @param portrait (bool, optional): Boolean flag set to `True` if the workflow
    #'              execution graph should be rendered in portrait orientation. Set
    #'              to `False`, if the graph should be rendered in landscape orientation. (default: False)
    #' @param max_events (int, optional): Specifies the number of events to be visualized
    #'              in the workflow execution graph. (default: 25000)
    render_progress = function(portrait=FALSE,
                               max_events=25000){
      events = self$list_events(max_items=max_events)
      widget = ExecutionGraphWidget$new(
        self$workflow$definition$to_json(),
        toJSON(events, auto_unbox = T),
        execution_arn=self$execution_arn)
      return(widget$show(portrait=portrait))
    },

    #' @description Get the input for the workflow execution.
    #' @param list or dict: Workflow execution input.
    get_input = function(){
      run_input = self$describe()[['input']]
      if(is.null(run_input))
        return(run_input)
      return(fromJSON(run_input))
    },


    #' @description Get the output for the workflow execution.
    #' @param wait (bool, optional): Boolean flag set to `True` if the call should wait for a running workflow execution to end before returning the output. Set to `False`, otherwise. Note that if the status is running, and `wait` is set to `True`, this will be a blocking call. (default: False)
    #' @return list or dict: Workflow execution output.
    get_output = function(wait=FALSE){
      while(wait && self$describe()[['status']] == 'RUNNING'){
        Sys.sleep(1)
      }
      output = self$describe()[["output"]]
      if (is.null(outpu))
        return(output)
      return(fromJSON(output))
    },

    #' @description format class
    format = function(){
      if(pkg_env$juptyer){
        return(sprintf('Execution: <a target="_blank" href="%s">%s</a>',
              create_sfn_execution_url(self$execution_arn), self$execution_arn))
      } else {
        return(sprintf("%s(execution_arn='%s', name='%s', status='%s', start_date='%s')",
              class(self)[1], self$execution_arn, self$name, self$status, self$start_date))
      }
    },

    #' @description print class
    print = function(){
      if(pkg_env$jupyter){
        display_html <- pkg_method("display_html","IRdisplay")
        display_html(self$format())
      } else {
        cat(self$format(), "\n")
      }
    }
  ),
  lock_objects=F
)
