# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/src/stepfunctions/workflow/widgets/workflows_table.py

#' @import R6
#' @importFrom glue glue

#' @include workflow_widgets_utils.R

WORKFLOW_TABLE_TEMPLATE = '
    <style>
        $$aws_table_css$$
        $$custom_css$$
    </style>
    <table class="table-widget">
        <thead>
            <tr>
                <th>Name</th>
                <th>Creation Date</th>
            </tr>
        </thead>
        <tbody>
            {table_rows}
        </tbody>
    </table>
'

WORKFLOW_TABLE_ROW_TEMPLATE = '
    <tr class="awsui-table-row">
        <td>
            <a href="$$state_machine_url$$" target="_blank" class="awsui">$$name$$</a>
        </td>
        <td>$$creation_date$$</td>
    </tr>
'

WORKFLOW_CSS_TEMPLATE = '
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

WorkflowsTableWidget = R6Class("WorkflowsTableWidget",
  public = list(
    initialize = function(workflows){
      table_rows = lapply(workflows, function(workflow){
        glue(WORKFLOW_TABLE_ROW_TEMPLATE,
             state_machine_url=create_sfn_workflow_url(workflow[['stateMachineArn']]),
             name=workflow[['name']],
             creation_date=format_time(workflow[['creationDate']]),
             .open = "$$",
             .close = "$$")
      })
      self$template = glue(WORKFLOW_TABLE_TEMPLATE, table_rows=paste(table_rows,collapse='\n'))
    },

    show = function(){
      return(glue(self$template,
        aws_table_css=AWS_TABLE_CSS,
        custom_css=WORKFLOW_CSS_TEMPLATE,
        .open = "$$",
        .close = "$$")
      )
    }
  ),
  lock_objects=F
)
