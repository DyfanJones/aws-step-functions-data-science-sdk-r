# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/src/stepfunctions/workflow/widgets/utils.py

AWS_SAGEMAKER_URL = "https://console.aws.amazon.com/sagemaker/home?region=%s#/%s/%s"
AWS_SFN_EXECUTIONS_DETAIL_URL = "https://console.aws.amazon.com/states/home?region=%s#/executions/details/%s"
AWS_SFN_STATE_MACHINE_URL = "https://console.aws.amazon.com/states/home?region=%s#/statemachines/view/%s"

AWS_TABLE_CSS = ('
    .table-widget {
        width: 100%;
        font-size: 14px;
        line-height: 28px;
        color: #545b64;
        border-spacing: 0;
        background-color: #fff;
        border-color: grey;
        background: #fafafa;
    }
    .table-widget thead th {
        text-align: left !important;
        color: #879596;
        padding: 0.3em 2em;
        border-bottom: 1px solid #eaeded;
        min-height: 4rem;
        line-height: 28px;
    }
    .table-widget thead th:first-of-type {
    }
    .table-widget td {
        overflow-wrap: break-word;
        padding: 0.4em 2em;
        line-height: 28px;
        text-align: left !important;
        background: #fff;
        border-bottom: 1px solid #eaeded;
        border-top: 1px solid transparent;
    }
    .table-widget td:before {
        content: "";
        height: 3rem;
    }
    a {
        cursor: pointer;
        text-decoration: none !important;
        color: #007dbc;
    }
    a:hover {
        text-decoration: underline !important;
    }
    a.disabled {
        color: black;
        cursor: default;
        pointer-events: none;
    }
    .hide {
        display: none;
    }
    pre {
        white-space: pre-wrap;
    }
')

format_time = function(timestamp){
  if (is.null(timestamp))
    return("-")
  time = strftime(timestamp, "%b %d, %Y %I:%M:%S")
  return(paste(time, strftime(timestamp, "%p")))
}

get_timestamp = function(date){
  return(as.POSIXct(date))
}

get_elapsed_ms = function(start_datetime,end_datetime){
  elapsed_time_seconds = (end_datetime - start_datetime)
  return(elapsed_time_seconds / 1000)
}

create_sfn_execution_url = function(execution_arn){
  arn_segments = unlist(strsplit(execution_arn, split=":"))
  region_name = arn_segments[4]
  return(sprintf(AWS_SFN_EXECUTIONS_DETAIL_URL, region_name, execution_arn))
}

create_sfn_workflow_url = function(state_machine_arn){
  arn_segments = unlist(strsplit(state_machine_arn, split=":"))
  region_name = arn_segments[4]
  return(sprintf(AWS_SFN_STATE_MACHINE_URL,region_name, state_machine_arn))
}

sagemaker_console_link = function(resource_type, resource){
  get_region = pkg_method("get_region", "paws.common")
  region_name = get_region()
  return(sprintf(AWS_SAGEMAKER_URL, region_name, resource_type, resource))
}
