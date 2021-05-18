# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/src/stepfunctions/template/pipeline/common.py

#' @import R6

#' @include steps_states.R
#' @include template_utils.R
#' @include utils.R

StepId = Enum(
  Train             = 'Training',
  CreateModel       = 'Create Model',
  ConfigureEndpoint = 'Configure Endpoint',
  Deploy            = 'Deploy',

  TrainPreprocessor       = 'Train Preprocessor',
  CreatePreprocessorModel = 'Create Preprocessor Model',
  TransformInput          = 'Transform Input',
  CreatePipelineModel     = 'Create Pipeline Model'
)


WorkflowTemplate = R6Class("WorkflowTemplate",
  public = list(
    initialize = function(s3_bucket, workflow, role, client, ...){
      self$workflow = workflow
      self$role = role
      self$s3_bucket = s3_bucket
    },

    render_graph = function(portrait=FALSE){
      return(self$workflow$render_graph(portrait=portrait))
    },

    get_workflow = function(){
      return(self$workflow)
    },

    build_workflow_definition = function(){
        stop("Not Implemented")
    },

    create =function(){
      return(self$workflow$create())
    },

    execute = function(...){
      stop("Not Implemented")
    },

    format = function(){
      cls_fmt = "%s(s3_bucket='%s', workflow='%s', role='%s')"
      return(sprintf(cls_fmt, class(self)[1], self$bucket,
             self$workflow, self$role))
    }
  ),
  private = list(
    .generate_timestamp = function(){
      return(strftime(Sys.time(),'%Y-%m-%d-%H-%M-%S'))
    },

    .extract_input_template = function(definition){
      input_template = list()

      for (step in definition$steps){
        if (inherits(step, "Task")){
          input_template[[step$state_id]] = step$parameters
          step$update_parameters(replace_parameters_with_context_object(step))
        }
      }
      return(input_template)
    }
  ),
  lock_objects=F
)
