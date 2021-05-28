# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/src/stepfunctions/steps/states.py

#' @include utils.R
#' @include steps_fields.R
#' @include inputs_placeholders.R

#' @import R6
#' @importFrom jsonlite toJSON
#' @importFrom tools toTitleCase

to_pascalcase = function(text){
  test = split_str(text, "_")
  return(paste0(tools::toTitleCase(test), collapse = ""))
}

#' @title Base class
#' @description Base class to abstract blocks used in Amazon States Language
#'              \url{https://states-language.net/spec.html}.
#' @export
Block = R6Class("Block",
  public = list(

    #' @description Initialize Block class
    #' @param ... : Field list to added to class
    initialize = function(...){
      kwargs = list(...)
      self$fields = kwargs
      if("comment" %in% names(kwargs))
        self$comment = kwargs$comment

      for(k in names(kwargs)){
        if(!self$is_field_allowed(k))
          stop(sprintf("Field '%s' is not supported.", k))
      }
    },

    #' @description Check if field is allowed
    #' @param field_name (str): field name to check with class's allowed field list
    is_field_allowed = function(field_name){
      return(field_name %in% self$allowed_fields())
    },

    #' @description allowed extra fields
    allowed_fields = function(){
      return(list())
    },

    #' @description Convert class to named list
    to_list= function(){
      result = list()
      fields_accepted_as_none = c('result_path', 'input_path', 'output_path')
      # Common fields
      for (k in names(self$fields)){
        v = self$fields[[k]]
        if (!is.null(v) || (k %in% fields_accepted_as_none)){
          k = to_pascalcase(k)
          if (k == to_pascalcase(Field$Parameters)){
            result[[k]] = private$.replace_placeholders(v)
          } else {
            result[[k]] = v
          }
        }
      }
      return(result)
    },

    #' @description Serialize to a JSON formatted string.
    #' @param pretty (bool, optional): Boolean flag set to `True` if JSON string
    #'              should be prettified. `False`, otherwise. (default: False)
    #' @return str: JSON formatted string representation of the block.
    to_json = function(pretty=FALSE){
      return(
        jsonlite::toJSON(
          self$to_list(),
          pretty = pretty,
          auto_unbox = TRUE)
      )
    },

    #' @description class formatting
    format = function(){
      cls = "%s(%s)"
      el_n = names(self$fields)
      elements = if(length(self$fields) > 0) paste(el_n, shQuote(unname(self$fields)), sep= "=", collapse = ", ") else ""
      return(sprintf(cls, class(self)[1], elements))
    }
  ),
  private = list(
    .replace_placeholders = function(params){
      if (!is_list_named(params))
        return(params)
      modified_parameters = list()
      for(k in names(params)){
        v = params[[k]]
        if (inherits(v, c("ExecutionInput", "StepInput"))){
          modified_key = sprintf("%s.$", k)
          modified_parameters[[modified_key]] = v$to_jsonpath()
        } else if (is_list_named(v)) {
            modified_parameters[[k]] = private$.replace_placeholders(v)
        } else if (inherits(v, "list")){
          modified_parameters[[k]] = lapply(v, private$.replace_placeholders)
        } else {
          modified_parameters[[k]] = v
        }
      }
      return(modified_parameters)
    }
  ),
  lock_objects = F
)

#' @title Retry base class
#' @description A class for creating a Retry block
#' @export
Retry = R6Class("Retry",
  inherit = Block,
  public = list(

    #' @description Initialize a Retry block.
    #' @param error_equals (list(str)): Non-empty list of strings, which match
    #'              Error Names \url{https://states-language.net/spec.html#error-names}.
    #'              When a state reports an error, the interpreter scans through
    #'              the retries and, when the Error Name appears in the value of
    #'              a retrier’s `error_equals` field, implements the retry policy
    #'              described in that retrier.
    #' @param interval_seconds (int, optional): Positive integer representing the
    #'              number of seconds before the first retry attempt. (default: 1)
    #' @param max_attempts (int, optional): Non-negative integer representing the
    #'              maximum number of retry attempts. (default: 3)
    #' @param backoff_rate (float, optional): A number which is the multiplier that
    #'              increases the retry interval on each attempt. (default: 2.0)
    #' @param ... : Extra field names to pass to Block class
    initialize = function(error_equals,
                          interval_seconds=NULL,
                          max_attempts=NULL,
                          backoff_rate=NULL,
                          ...){
      error_equals = as.list(error_equals)
      kwargs = c(
        error_equals=list(error_equals),
        interval_seconds=interval_seconds,
        max_attempts=max_attempts,
        backoff_rate=backoff_rate,
        list(...))
      do.call(super$initialize, kwargs)
    },

    #' @description allowed extra fields
    allowed_fields = function(){
      return(list(
        Field$ErrorEquals,
        Field$IntervalSeconds,
        Field$MaxAttempts,
        Field$BackoffRate)
      )
    }
  ),
  lock_objects = F
)

#' @title Catch base Class
#' @description A class for creating a Catch block.
#' @export
Catch = R6Class("Catch",
  inherit = Block,
  public=list(

    #' @description Initialize a Catch block.
    #' @param next_step (State or Chain): Next state or chain to transition to.
    #' @param error_equals (list(str)): Non-empty list of strings, which match
    #'              Error Names \url{https://states-language.net/spec.html#error-names}.
    #'              When a state reports an error, the interpreter scans through the catchers
    #'              and, when the Error Name appears in the value of of a catcher's `error_equals`
    #'              field, transitions to the `next_step` described in the catcher.
    #' @param ... : Extra field names to pass to Block class
    initialize = function(next_step,
                          error_equals,
                          ...){
      error_equals = as.list(error_equals)
      kwargs = c(error_equals=list(error_equals), list(...))
      do.call(super$initialize, kwargs)
      self$next_step = next_step
    },

    #' @description allowed extra fields
    allowed_fields = function(){
      return(list(
        Field$ErrorEquals,
        Field$ResultPath)
      )
    },

    #' @description Convert class to list ready to be translated for
    #'              Amazon States Language \url{https://states-language.net/spec.html}.
    to_list = function(){
      result = super$to_list()
      result[["Next"]] = self$next_step$state_id
      return(result)
    }
  ),
  lock_objects = F
)

#' @title State base class
#' @description A class for creating a State block.
#' @export
State = R6Class("State",
  inherit = Block,
  public = list(

    #' @description Initialize State base class
    #' @param state_id (str): State name whose length **must be** less than or
    #'              equal to 128 unicode characters. State names **must be**
    #'              unique within the scope of the whole state machine.
    #' @param state_type (str): Type of the state. (Allowed values: `'Pass'`,
    #'              `'Succeed'`, `'Fail'`, `'Wait'`, `'Task'`, `'Choice'`, `'Parallel'`, `'Map'`).
    #' @param output_schema (list): Expected output schema for the State. This is
    #'              used to validate placeholder inputs used by the next state in
    #'              the state machine. (default: None)
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
    #' @param ... : Extra field names to pass to Block class
    initialize = function(state_id,
                          state_type,
                          output_schema=NULL,
                          comment=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){

      kwargs = list(
        comment=comment,
        input_path=input_path,
        parameters=parameters,
        result_path=result_path,
        output_path=output_path,
        ...)
      kwargs = Filter(Negate(is.null), kwargs)
      do.call(super$initialize, kwargs)

      self$fields[["type"]] = state_type
      self$type = state_type
      self$state_type = state_type
      self$state_id = state_id
      self$output_schema = output_schema
      self$step_output = StepInput$new(schema=output_schema)
      self$retries = list()
      self$catches = list()
      self$next_step = NULL
      self$in_chain = NULL
    },

    #' @description allowed extra fields
    allowed_fields = function(){
      return(list(
        Field$Comment,
        Field$InputPath,
        Field$OutputPath,
        Field$Parameters,
        Field$ResultPath)
      )
    },

    #' @description Update `parameters` field in the state, if supported.
    #' @param params (list): The value of this field becomes the effective input for the state.
    update_parameters = function(params){
      if (Field$Parameters %in% self$allowed_fields())
        self$fields[[Field$Parameters]] = params
    },

    #' @description Specify the next state or chain to transition to.
    #' @param next_step (State or Chain): Next state or chain to transition to.
    #' @return State or Chain: Next state or chain that will be transitioned to.
    .next = function(next_step){
      if (self$type %in% c('Succeed', 'Fail'))
        stop(sprintf(
          'Unexpected State instance `%s`, State type `%s` does not support method `.next`.',
          class(next_step)[1], self$type))

      # By design, Choice states do not have the Next field. When used in a chain, the subsequent step becomes the
      # default choice that executes if none of the specified rules match.
      # See language spec for more info: https://states-language.net/spec.html#choice-state
      if (self$type == 'Choice'){
        if (!is.null(self$default))
          LOGGER$warn(
            "Chaining Choice state: Overwriting (%s)'s current default_choice (%s) with (%s)",
            self$state_id, self$default$state_id, next_step$state_id)
        self$default_choice(next_step)
        return(self$default)
      }
      self$next_step = next_step
      return(self$next_step)
    },

    #' @description Get the placeholder collection for the State's output.
    #' @return StepInput: Placeholder collection representing the State's output,
    #'              and consequently the input to the next state in the workflow (if one exists).
    output = function(){
      return(self$step_output)
    },

    #' @description placeholder
    #' @param visitor placeholder
    accept = function(visitor){
      if (visitor$is_visited(self))
        return(NULL)

      visitor$visit(self)
      if (!is.null(self$next_step))
        self$next_step$accept(visitor)
      for (catch in self$catches){
        catch$next_step$accept(visitor)
      }
    },

    #' @description Add a Retry block to the tail end of the list of retriers for the state.
    #' @param retry (Retry): Retry block to add.
    add_retry = function(retry){
      if (Field$Retry %in% self$allowed_fields()){
        self$retries = c(self$retries, retry)
      } else {
        stop(sprintf("%s state does not support retry field.",
            class(self)[1]))
      }
    },

    #' @description Add a Catch block to the tail end of the list of catchers for the state.
    #' @param catch (Catch): Catch block to add.
    add_catch = function(catch){
      if (Field$Catch %in% self$allowed_fields()){
        self$catches = c(self$catches, catch)
      } else {
          stop(sprintf(
            "%s state does not support catch field.",class(self)[1]))
      }
    },

    #' @description Convert class to list ready to be translated for
    #'              Amazon States Language \url{https://states-language.net/spec.html}.
    to_list = function(){
      result = super$to_list()

      # Next step
      if (!is.null(self$next_step)){
        result[["Next"]] = self$next_step$state_id
      } else if (!(self$state_type %in% c('Succeed', 'Fail', 'Choice'))){
        result[["End"]] = TRUE
      }

      # Retry and catch
      if (!islistempty(self$retries) && self$is_field_allowed(Field$Retry))
        result[["Retry"]] = lapply(self$retries, function(retry) retry$to_list())
      if (!islistempty(self$catches) && self$is_field_allowed(Field$Catch))
        result[["Catch"]] = lapply(self$catches, function(catch) catch$to_list())
      return(result)
    },

    #' @description class formatting
    format = function(){
      return(paste(self$state_id, super$format()))
    }
  ),
  lock_objects = F
)

#' @title Pass State Class
#' @description Pass State simply passes its input to its output, performing no work.
#'              Pass States are useful when constructing and debugging state machines.
#' @export
Pass = R6Class("Pass",
  inherit = State,
  public = list(

    #' @description Initialize Pass state class
    #' @param state_id (str): State name whose length **must be** less than or
    #'              equal to 128 unicode characters. State names **must be** unique
    #'              within the scope of the whole state machine.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param result (str, optional): If present, its value is treated as the output
    #'              of a virtual task, and placed as prescribed by the `result_path` field,
    #'              if any, to be passed on to the next state. If `result` is not
    #'              provided, the output is the input.
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra field names to pass to Block class
    initialize = function(state_id,
                          comment = NULL,
                          input_path = NULL,
                          parameters = NULL,
                          result_path = NULL,
                          result = NULL,
                          output_path = NULL,
                          ...){
      kwargs = c(as.list(environment()), state_type = "Pass", list(...))

      do.call(super$initialize, kwargs)
    },

    #' @description allowed extra fields
    allowed_fields = function(){
      return(list(
        Field$Comment,
        Field$InputPath,
        Field$OutputPath,
        Field$Parameters,
        Field$ResultPath,
        Field$Result)
      )
    }
  ),
  lock_objects = F
)

#' @title Succeed State Class
#' @description Succeed State terminates a state machine successfully. The Succeed State is a useful target for
#'              :R:class:`Choice`-state branches that don't do anything but terminate the machine.
#' @export
Succeed = R6Class("Succeed",
  inherit=State,
  public=list(

    #' @description Initialize Succeed State class
    #' @param state_id (str): State name whose length **must be** less than or
    #'              equal to 128 unicode characters. State names **must be** unique
    #'              within the scope of the whole state machine.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output, producing
    #'              the effective output which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra field names to pass to Block class
    initialize = function(state_id,
                          comment=NULL,
                          input_path=NULL,
                          output_path=NULL){
      kwargs = c(as.list(environment()), state_type = "Succeed")

      do.call(super$initialize, kwargs)
    },

    #' @description allowed extra fields
    allowed_fields = function(){
      return(list(
          Field$Comment,
          Field$InputPath,
          Field$OutputPath)
      )
    }
  ),
  lock_objects = F
)

#' @title Fail State Class
#' @description Fail State terminates the machine and marks it as a failure.
#' @export
Fail = R6Class("Fail",
  inherit = State,
  public = list(

    #' @description Initialize Fail state class
    #' @param state_id (str): State name whose length **must be** less than or
    #'              equal to 128 unicode characters. State names **must be** unique
    #'              within the scope of the whole state machine.
    #' @param error (str): Error name that can be used for error handling (retry/catch),
    #'              operational, or diagnostic purposes.
    #' @param cause (str): Human-readable message describing the cause of the failure/error.
    #' @param comment (str, optional): Human-readable comment or description. (default: None).
    #' @param ... : Extra field names to pass to Block class
    initialize = function(state_id,
                          error=NULL,
                          cause=NULL,
                          comment=NULL,
                          ...){
      kwargs = c(as.list(environment()), state_type = "Fail", list(...))

      do.call(super$initialize, kwargs)
      self$error=error
      self$cause=cause
    },

    #' @description allowed extra fields
    allowed_fields = function(){
      return(list(
        Field$Comment,
        Field$Error,
        Field$Cause)
      )
    }
  ),
  lock_object=F
)

#' @title Wait State Class
#' @description Wait state causes the interpreter to delay the machine from continuing
#'              for a specified time. The time can be specified as a wait duration,
#'              specified in seconds, or an absolute expiry time, specified as an ISO-8601
#'              extended offset date-time format string.
#' @export
Wait = R6Class("Wait",
  inherit = State,
  public = list(

    #' @description The Wait state **must contain exactly one** of `seconds`,
    #'              `seconds_path`, `timestamp`, or `timestamp_path`.
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'              to 128 unicode characters. State names **must be** unique within
    #'              the scope of the whole state machine.
    #' @param seconds (int): Wait duration specified in seconds.
    #' @param seconds_path (str): Path applied to the state's input to select the
    #'              wait duration in seconds.
    #' @param timestamp (str): Absolute expiry time, specified as an ISO-8601 extended
    #'              offset date-time format string.
    #' @param timestamp_path (str): Path applied to the state's input to select the
    #'              timestamp to be used for wait duration.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param input_path (str, optional): Path applied to the state’s raw input to select
    #'              some or all of it; that selection is used by the state. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output, producing
    #'              the effective output which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra field names to pass to Block class
    initialize = function(state_id,
                          seconds=NULL,
                          seconds_path=NULL,
                          timestamp=NULL,
                          timestamp_path=NULL,
                          comment=NULL,
                          input_path=NULL,
                          output_path=NULL,
                          ...){
      if (length(Filter(Negate(is.null),list(seconds, timestamp, timestamp_path, seconds_path))) != 1)
        stop(
          "The Wait state MUST contain exactly one of 'seconds', 'seconds_path', 'timestamp' or 'timestamp_path'.")

      kwargs = c(as.list(environment()), state_type = "Wait", list(...))
      do.call(super$initialize, kwargs)

      self$seconds=seconds
      self$seconds_path=seconds_path
      self$timestamp=timestamp
      self$timestamp_path=timestamp_path
      self$input_path=input_path
      self$output_path=output_path
    },

    #' @description allowed extra fields
    allowed_fields = function(){
      return(list(
        Field$Comment,
        Field$InputPath,
        Field$OutputPath,
        Field$Seconds,
        Field$Timestamp,
        Field$SecondsPath,
        Field$TimestampPath)
      )
    }
  ),
  lock_objects = F
)

#'@title Choice State class
#' @description Choice state adds branching logic to a state machine. The state
#'              holds a list of *rule* and *next_step* pairs. The interpreter attempts
#'              pattern-matches against the rules in list order and transitions to the
#'              state or chain specified in the *next_step* field on the first *rule* where
#'              there is an exact match between the input value and a member of the
#'              comparison-operator array. When used in a chain, the subsequent step
#'              becomes the default choice that executes if none of the specified rules match.
#' @export
Choice = R6Class("Choice",
  inherit = State,
  public = list(

    #' @description Initialize Choice class
    #' @param state_id (str): State name whose length **must be** less than
    #'              or equal to 128 unicode characters. State names **must be**
    #'              unique within the scope of the whole state machine.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param input_path (str, optional): Path applied to the state’s raw input
    #'              to select some or all of it; that selection is used by the state. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output,
    #'              producing the effective output which serves as the raw input
    #'              for the next state. (default: '$')
    #' @param ... : Extra field names to pass to Block class
    initialize = function(state_id,
                          comment=NULL,
                          input_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = c(as.list(environment()), state_type = "Choice", list(...))

      do.call(super$initialize, kwargs)
      self$choices = list()
      self$default = NULL
      self$input_path=input_path
      self$output_path=output_path
    },

    #' @description allowed extra fields
    allowed_fields = function(){
      return(list(
        Field$Comment,
        Field$InputPath,
        Field$OutputPath)
      )
    },

    #' @description Add a *rule*, *next_step* pair to the choice state.
    #' @param rule (:R:class:`stepfunctions.steps.choice_rule.BaseRule`): Rule to pattern match the input against.
    #' @param next_step (State or Chain): Next state or chain to transition to, if `rule` is matches with the input.
    add_choice = function(rule, next_step){
      self$choices = c(self$choices, list(list(rule, next_step)))
    },

    #' @description Add a default step to the choice state.
    #'              The default step executes if none of the specified rules match.
    #' @param next_step (State or Chain): Next state or chain to transition to, if none of the specified rules match.
    default_choice = function(next_step){
      self$default = next_step
    },

    #' @description Convert class to list ready to be translated for
    #'              Amazon States Language \url{https://states-language.net/spec.html}.
    to_list = function(){
      result = super$to_list()

      serialized_choices = list()
      for (ll in self$choices){
        serialized_choice = ll[[1]]$to_list()
        serialized_choice[["Next"]] = ll[[2]]$state_id
        serialized_choices = c(serialized_choices, list(serialized_choice))
        }
      result[["Choices"]] = serialized_choices

      if (!is.null(self$default))
        result[["Default"]] = self$default$state_id
      return(result)
    },

    #' @description placeholder
    #' @param visitor placeholder
    accept = function(visitor){
      if (visitor$is_visited(self))
        return(NULL)

      visitor$visit(self)
      if (!is.null(self$default))
        self$default$accept(visitor)

      for (ll in self$choices){
        ll[[2]]$accept(visitor)
      }
    }
  ),
  lock_object=F
)

#' @title Parallel State causes parallel execution of "branches".
#' @description A Parallel state causes the interpreter to execute each branch
#'              as concurrently as possible, and wait until each branch terminates
#'              (reaches a terminal state) before processing the next state in the Chain.
#' @export
Parallel = R6Class("Parallel",
  inherit = State,
  public = list(

    #' @description Initialize Parallel state class
    #' @param state_id (str): State name whose length **must be** less than or
    #'              equal to 128 unicode characters. State names **must be** unique
    #'              within the scope of the whole state machine.
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param input_path (str, optional): Path applied to the state’s raw input
    #'              to select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the
    #'              effective input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra field names to pass to Block class
    initialize = function(state_id,
                          comment=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = c(as.list(environment()), state_type = "Parallel", list(...))

      self$input_path=input_path
      self$result_path=result_path
      self$output_path=output_path

      do.call(super$initialize, kwargs)

      self$branches = list()
    },

    #' @description allowed extra fields
    allowed_fields = function(){
      return(list(
        Field$Comment,
        Field$InputPath,
        Field$OutputPath,
        Field$Parameters,
        Field$ResultPath,
        Field$Retry,
        Field$Catch)
      )
    },

    #' @description Add a `State` or `Chain` as a branch to the Parallel state.
    #' @param branch (State or Chain): State or Chain to attach as a branch to the Parallel state.
    add_branch = function(branch){
      self$branches = c(self$branches, branch)
    },

    #' @description Convert class to list ready to be translated for
    #'              Amazon States Language \url{https://states-language.net/spec.html}.
    to_list = function(){
      result = super$to_list()
      result[["Branches"]] = lapply(
        self$branches, function(branch) {
          graph_state = Graph$new(branch)
          graph_state$to_list()
        })
      return(result)
    }
  ),
  lock_object=F
)

#' @title Map state provides the ability to dynamically iterate over a state/subgraph for each entry in a list.
#' @description A Map state can accept an input with a list of items,
#'              execute a state or chain for each item in the list, and return
#'              a list, with all corresponding results of each execution, as its output.
#' @export
Map = R6Class("Map",
  inherit=State,
  public = list(

    #' @description Initialize Map state class
    #' @param state_id (str): State name whose length **must be** less than or
    #'              equal to 128 unicode characters. State names **must be** unique
    #'              within the scope of the whole state machine.
    #' @param iterator (State or Chain): State or chain to execute for each of the items in `items_path`.
    #' @param items_path (str, optional): Path in the input for items to iterate over. (default: '$')
    #' @param max_concurrency (int, optional): Maximum number of iterations to have
    #'              running at any given point in time. (default: 0)
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param input_path (str, optional): Path applied to the state’s raw input to
    #'              select some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra field names to pass to Block class
    initialize = function(state_id,
                          iterator,
                          items_path=NULL,
                          max_concurrency=NULL,
                          comment=NULL,
                          input_path=NULL,
                          parameters=NULL,
                          result_path=NULL,
                          output_path=NULL,
                          ...){
      kwargs = c(as.list(environment()), state_type = "Map", list(...))

      self$iterator=iterator
      self$items_path=items_path
      self$max_concurrency=max_concurrency
      self$input_path=input_path
      self$result_path=result_path
      self$output_path=output_path

      do.call(super$initialize, kwargs)
    },

    #' @description Attach `State` or `Chain` as iterator to the Map state, that
    #'              will execute for each of the items in `items_path`. If an iterator
    #'              was attached previously with the Map state, it will be replaced.
    #' @param iterator (State or Chain): State or Chain to attach as iterator to the Map state.
    attach_iterator = function(iterator){
      self$iterator = iterator
    },

    #' @description allowed extra fields
    allowed_fields = function(){
      return(list(
        Field$Comment,
        Field$InputPath,
        Field$OutputPath,
        Field$Parameters,
        Field$ResultPath,
        Field$Retry,
        Field$Catch,
        Field$ItemsPath,
        Field$Iterator,
        Field$MaxConcurrency)
      )
    },

    #' @description Convert class to named list
    to_list = function(){
      result = super$to_list()
      graph_state = Graph$new(self$iterator)
      result[["Iterator"]] = graph_state$to_list()
      return(result)
    }
  ),
  lock_object=F
)

#' @title Task state class
#' @description Task State causes the interpreter to execute the work identified by the state’s `resource` field.
#' @export
Task = R6Class("Task",
  inherit = State,
  public = list(

    #' @description Initialize Task state class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'              to 128 unicode characters. State names **must be** unique within
    #'              the scope of the whole state machine.
    #' @param resource (str): A URI that uniquely identifies the specific task to
    #'              execute. The States language does not constrain the URI scheme
    #'              nor any other part of the URI.
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
    #' @param comment (str, optional): Human-readable comment or description. (default: None)
    #' @param input_path (str, optional): Path applied to the state’s raw input to select
    #'              some or all of it; that selection is used by the state. (default: '$')
    #' @param parameters (list, optional): The value of this field becomes the effective
    #'              input for the state.
    #' @param result_path (str, optional): Path specifying the raw input’s combination
    #'              with or replacement by the state’s result. (default: '$')
    #' @param output_path (str, optional): Path applied to the state’s output after
    #'              the application of `result_path`, producing the effective output
    #'              which serves as the raw input for the next state. (default: '$')
    #' @param ... : Extra field names to pass to Block class
    initialize = function(state_id,
                          resource,
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
      kwargs = c(as.list(environment()), state_type = "Task", list(...))

      if (!is.null(timeout_seconds) && !is.null(timeout_seconds_path))
        stop("Only one of 'timeout_seconds' or 'timeout_seconds_path' can be provided.")

      if (!is.null(heartbeat_seconds) && !is.null(heartbeat_seconds_path))
        stop("Only one of 'heartbeat_seconds' or 'heartbeat_seconds_path' can be provided.")

      do.call(super$initialize, kwargs)

      self$input_path=input_path
      self$result_path=result_path
      self$output_path=output_path
    },

    #' @description allowed extra fields
    allowed_fields = function(){
      return(list(
        Field$Comment,
        Field$InputPath,
        Field$OutputPath,
        Field$Parameters,
        Field$ResultPath,
        Field$TimeoutSeconds,
        Field$TimeoutSecondsPath,
        Field$HeartbeatSeconds,
        Field$HeartbeatSecondsPath,
        Field$Resource,
        Field$Retry,
        Field$Catch)
      )
    }
  ),
  lock_object=F
)

#' @title Chain class
#' @description Chain is a logical group of states, that resembles a linked list.
#'              The start state is the head of the *Chain* and the end state is the
#'              tail of the *Chain*.
#' @export
Chain = R6Class("Chain",
  public = list(

    #' @description Initialise Chain class
    #' @param steps (list(State), optional): List of states to be chained in-order. (default: list())
    #' @examples
    #' library(stepfunctions)
    #' s1_pass = Pass$new('Step - One')
    #' s2_pass = Pass$new('Step - two')
    #' s3_pass = Pass$new('Step - three')
    #' chain1 = Chain$new(c(s1_pass, s2_pass))
    #' chain2 = Chain$new(c(s3_pass, chain1))
    initialize = function(steps=list()){
      if(!is.list(steps))
        stop("Chain takes a 'list' of steps. You provided an input that is not a list.")

      self$steps = list()
      steps_expanded = unlist(lapply(steps, function(step) if(inherits(step, "Chain")) step$steps else step))

      if(length(unique(steps_expanded)) != length(steps_expanded))
        stop("Duplicate states in the chain.")

      base::Map(self$append, steps_expanded)
    },

    #' @description Add a state at the tail end of the chain.
    #' @param step (State): State to insert at the tail end of the chain.
    append = function(step){
      if (length(self$steps) == 0){
        self$steps = c(self$steps, step)
      } else {
        if (any(sapply(self$steps, function(i) identical(step, i))))
          stop(sprintf(
            "State '%s' is already inside this chain. A chain cannot have duplicate states.",
            step$state_id))
        last_step = self$steps[[length(self$steps)]]
        last_step$.next(step)
        self$steps = c(self$steps, step)
      }
    },

    #' @description placeholder
    #' @param visitor placeholder
    accept = function(visitor){
      for(step in self$steps){
        step$accept(visitor)}
    },

    #' @description class formatting
    format = function(){
      cls_fmt = "%s(steps=[%s])"
      step_fmt = paste(lapply(self$steps, function(i) i$format()), sep = ", ", collapse=", ")
      return(sprintf(cls_fmt, class(self)[1], step_fmt))
    }
  ),
  active = list(

    #' @field state_id
    #' Chain class state_id
    state_id = function(){
      if (length(self$steps) == 0)
        stop('The chain is empty',call.=F)
      return(self$steps[[1]]$state_id)
    }
  ),
  lock_object=F
)

# GraphVisitor class
GraphVisitor = R6Class("GraphVisitor",
  public = list(
    initialize = function(){
      self$states = list()
    },

    is_visited = function(state){
      return(state$state_id %in% names(self$states))
    },

    visit = function(state){
      self$states[[state$state_id]] = state$to_list()
    }
  ),
  lock_object=F
)


ValidationVisitor = R6Class("ValidationVisitor",
  public = list(
    initialize = function(){
      self$states = list()
    },

    is_visited = function(state){
      if (identical(self$states[[state$state_id]], state$to_list()))
        return(TRUE)
      else
        return(FALSE)
    },

    visit = function(state){
      if (state$state_id %in% names(self$states)){
        stop(sprintf(
          "Each state in a workflow must have a unique state id. Found duplicate state id '%s' in workflow.",
          state$state_id))}
      self$states[[state$state_id]] = state$to_list()
      if (is.null(state$next_step))
        return(NULL)
      if (is.null(state$next_step$fields) || !(Field$Parameters %in% names(state$next_step$fields)))
        return(NULL)
      params = state$next_step$fields[[Field$Parameters]]
      ll = private$.validate_next_step_params(params, state$step_output)
      names(ll) = c("valid", "invalid_param_name")
      if (is.null(ll$valid))
        stop(sprintf('State \'%s\' is using an illegal placeholder for the \'%s\' parameter.',
             state$next_step$state_id, ll$invalid_param_name))
    }
  ),
  private = list(

    .validate_next_step_params = function(params, step_output){
      for (k in names(params)){
        v = params[[k]]
        if (inherits(v, "StepInput")){
          if (!identical(v, step_output) && !step_output$contains(v))
            return(list(FALSE, k))
        } else if (inherits(v, "list")){
          ll = private$.validate_next_step_params(v, step_output)
          names(ll) = c("valid", "invalid_param_name")
          if (!is.null(ll$valid))
            return(list(ll$valid, ll$invalid_param_name))
        }
      }
      return(list(TRUE, NULL))
    }
  ),
  lock_objects=F
)

#' @title Graph class
#' @export
Graph = R6Class("Graph",
  inherit=Block,
  public = list(

    #' @description Initialize Graph class
    #' @param branch (State, Chain):
    #' @param ... : Extra field names to pass to Block class
    initialize = function(branch,
                          ...){
      if (!inherits(branch, c("State", "Chain")))
        stop(sprintf(
        'Expected branch to be a State or a Chain, but got `%s`',
        deparse(substitute(branch))))

      super$initialize(...)

      self$branch = branch
      self$states = list()
      self$build_graph(branch)
    },

    #' @description allowed extra fields
    allowed_fields = function(){
      return(list(
        Field$TimeoutSeconds,
        Field$Comment,
        Field$Version)
      )
    },

    #' @description check if state is contained in class
    #' @param state :
    contains = function(state){
      return((self$states[[state$state_id]] %||% FALSE))
    },


    #' @description Create Graph
    #' @param state :
    build_graph = function(state){
      graph_visitor = GraphVisitor$new()
      validation_visitor = ValidationVisitor$new()
      state$accept(validation_visitor)
      state$accept(graph_visitor)
      self$states = graph_visitor$states
    },

    #' @description Convert class to list ready to be translated for
    #'              Amazon States Language \url{https://states-language.net/spec.html}.
    to_list = function(){
      result = super$to_list()
      result[['StartAt']] = self$branch$state_id
      result[['States']] = self$states
      return(result)
    }
  ),
  lock_objects=F
)

#' @title FrozenGraph class
#' @export
FrozenGraph = R6Class("FrozenGraph",
  inherit = Graph,
  public = list(

    #' @description Initialize FrozenGraph class
    #' @param definition (list):
    initialize = function(definition){
      if(!is.list(definition))
        stop(sprintf(
          "Expected definition to be a list, but got `%s`.", class(definition)))
      self$definition = definition
    },

    #' @description Convert class to list ready to be translated for
    #'              Amazon States Language \url{https://states-language.net/spec.html}.
    to_list = function(){
      return(self$definition)
    },

    #' @description Read in json definitions and output FrozenGraph class
    #' @param json_definition (list):
    from_json = function(json_definition){
      return(FrozenGraph$new(definition=fromJSON(json_definition)))
    }
  ),
  lock_objects=F
)
