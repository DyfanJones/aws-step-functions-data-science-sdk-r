# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/src/stepfunctions/steps/integration_resources.py

#' @include utils.R
#' @include steps_utils.R

# Integration pattern enum classes for task integration resource arn builder
IntegrationPattern = Enum(
  WaitForTaskToken = "waitForTaskToken",
  WaitForCompletion = "sync",
  RequestResponse = ""
)

# ARN builder for task integration
# Args:
#   service (str): The service name for the service integration
# api (str): The api of the service integration
# integration_pattern (IntegrationPattern, optional): The integration pattern for
# the task. (Default: IntegrationPattern.RequestResponse)
get_service_integration_arn = function(service,
                                       api,
                                       integration_pattern=IntegrationPattern$RequestResponse){
  region = get_region()
  arn = ""
  if (integration_pattern == IntegrationPattern$RequestResponse)
    arn = sprintf("arn:%s:states:::%s:%s", get_aws_partition(region), service, api)
  else
    arn = sprintf("arn:%s:states:::%s:%s.%s", get_aws_partition(region), service, api, integration_pattern)
  return(arn)
}
