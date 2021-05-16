# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/src/stepfunctions/workflow/cloudformation.py

#' @importFrom jsonlite toJSON
#' @importFrom yaml as.yaml
#' @import lgr

#' @include stepfunctions.R

CLOUDFORMATION_BASE_TEMPLATE = list(
  "AWSTemplateFormatVersion"='2010-09-09',
  "Description"=NULL,
  "Resources"=list(
    "StateMachineComponent"=list(
      "Type"="AWS::StepFunctions::StateMachine",
      "Properties"=list(
        "StateMachineName"=NULL,
        "DefinitionString"=NULL,
        "RoleArn"=NULL
      )
    )
  )
)

build_cloudformation_template = function(workflow){
  LOGGER$warn(paste('To reuse the CloudFormation template in different regions,',
                    'please make sure to update the region specific AWS resources',
                    'in the StateMachine definition.'))

  template = CLOUDFORMATION_BASE_TEMPLATE

  template[["Description"]] = "CloudFormation template for AWS Step Functions - State Machine"
  template[["Resources"]][["StateMachineComponent"]][["Properties"]][["StateMachineName"]] = workflow$name

  definition = workflow$definition$to_list()

  template[["Resources"]][["StateMachineComponent"]][["Properties"]][["DefinitionString"]] = (
    prettify(toJSON(definition, auto_unbox=T), indent=2))
  template[["Resources"]][["StateMachineComponent"]][["Properties"]][["RoleArn"]] = workflow$role

  return(as.yaml(template))
}
