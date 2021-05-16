# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/src/stepfunctions/workflow/widgets/executions_table.py

#' @import R6
#' @importFrom glue glue

#' @include workflow_widgets_utils.R

EXECUTION_TABLE_TEMPLATE = '
    <style>
        $$aws_table_css$$
        $$custom_css$$
    </style>
    <table class="table-widget">
        <thead>
            <tr>
                <th>Name</th>
                <th>Status</th>
                <th>Started</th>
                <th>End Time</th>
            </tr>
        </thead>
        <tbody>
            {table_rows}
        </tbody>
    </table>
'

EXECUTION_TABLE_ROW_TEMPLATE = '
    <tr class="awsui-table-row">
        <td>
            <a href="$$execution_url$$" target="_blank" class="awsui">$$name$$</a>
        </td>
        <td>$$status$$</td>
        <td>$$start_date$$</td>
        <td>$$stop_date$$</td>
    </tr>
'

EXECUTION_CSS_TEMPLATE = '
    * {
        box-sizing: border-box;
    }
    .table-widget {
        min-width: 100%;
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
    .table-widget td {
        /* padding: 24px 18px; */
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
    .table-widget .clickable-cell {
        cursor: pointer;
    }
    .hide {
        display: none;
    }
    .triangle-right {
        width: 0;
        height: 0;
        border-top: 5px solid transparent;
        border-left: 8px solid #545b64;
        border-bottom: 5px solid transparent;
        margin-right: 5px;
    }
    a.awsui {
        text-decoration: none !important;
        color: #007dbc;
    }
    a.awsui:hover {
        text-decoration: underline !important;
    }
'

ExecutionsTableWidget = R6Class("ExecutionsTableWidget",
  public = list(
    initialize = function(executions){
      table_rows = lapply(executions, function(execution){
        glue(EXECUTION_TABLE_ROW_TEMPLATE,
             execution_url=create_sfn_execution_url(execution.execution_arn),
             name=execution$name,
             status=execution$status,
             start_date=format_time(execution$start_date),
             stop_date=format_time(execution$stop_date),
             .open = "$$",
             .close = "$$")
      })

      self$template = glue(EXECUTION_TABLE_TEMPLATE, table_rows=paste(table_rows, collapse='\n'))
    },

    show = function(){
      return (glue(self$template,
        aws_table_css=AWS_TABLE_CSS,
        custom_css=EXECUTION_CSS_TEMPLATE,
        .open = "$$",
        .close = "$$")
      )
    }
  ),
  lock_objects=F
)

