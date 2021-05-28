# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/src/stepfunctions/workflow/widgets/graph.py

#' @import R6
#' @import jsonlite
#' @import lgr
#' @importFrom glue glue

#' @include workflow_widgets_utils.R

JSLIB_URL = 'https://do0of8uwbahzz.cloudfront.net/sfn'
CSS_URL = 'https://do0of8uwbahzz.cloudfront.net/graph.css'

HTML_TEMPLATE = '
<link rel="stylesheet" type="text/css" href="$$css$$">
<div id="$$element_id$$" class="workflowgraph">
    $$graph_legend_template$$
    <svg></svg>
    {console_snippet}
</div>
<script type="text/javascript">
{code_snippet}
</script>
'

EXECUTION_URL_TEMPLATE = '<a href="$$console$$" target="_blank"> Inspect in AWS Step Functions </a>'

WORKFLOW_GRAPH_SCRIPT_TEMPLATE = "
require.config({
    paths: {
        sfn: \"$$jslib$$\",
    }
});
require(['sfn'], function(sfn) {
    var element = document.getElementById('$$element_id$$')
    var options = {
        width: parseFloat(getComputedStyle(element, null).width.replace(\"px\", \"\")),
        height: 600,
        layout: '$$layout$$',
        resizeHeight: true
    };
    var definition = $$definition$$;
    var elementId = '#$$element_id$$';
    var graph = new sfn.StateMachineGraph(definition, elementId, options);
    graph.render();
});
"

EXECUTION_GRAPH_SCRIPT_TEMPLATE = "
require.config({
    paths: {
        sfn: \"$$jslib$$\",
    }
});
require(['sfn'], function(sfn) {
    var element = document.getElementById('$$element_id$$')
    var options = {
        width: parseFloat(getComputedStyle(element, null).width.replace(\"px\", \"\")),
        height: 1000,
        layout: '$$layout$$',
        resizeHeight: true
    };
    var definition = $$definition$$;
    var elementId = '#$$element_id$$';
    var events = { 'events': $$events$$ };
    var graph = new sfn.StateMachineExecutionGraph(definition, events, elementId, options);
    graph.render();
});
"

EXECUTION_GRAPH_LEGEND_TEMPLATE = '
    <style>
        .graph-legend ul {
            list-style-type: none;
            padding: 10px;
            padding-left: 0;
            margin: 0;
            position: absolute;
            top: 0;
            background: transparent;
        }
        .graph-legend li {
            margin-left: 10px;
            display: inline-block;
        }
        .graph-legend li > div {
            width: 10px;
            height: 10px;
            display: inline-block;
        }
        .graph-legend .success { background-color: #2BD62E }
        .graph-legend .failed { background-color: #DE322F }
        .graph-legend .cancelled { background-color: #DDDDDD }
        .graph-legend .in-progress { background-color: #53C9ED }
        .graph-legend .caught-error { background-color: #FFA500 }
    </style>
    <div class="graph-legend">
        <ul>
            <li>
                <div class="success"></div>
                <span>Success</span>
            </li>
            <li>
                <div class="failed"></div>
                <span>Failed</span>
            </li>
            <li>
                <div class="cancelled"></div>
                <span>Cancelled</span>
            </li>
            <li>
                <div class="in-progress"></div>
                <span>In Progress</span>
            </li>
            <li>
                <div class="caught-error"></div>
                <span>Caught Error</span>
            </li>
        </ul>
    </div>
'

WorkflowGraphWidget = R6Class("WorkflowGraphWidget",
  public = list(

      initialize = function(json_definition){
        self$json_definition = json_definition
        self$element_id = sprintf('graph-%d', as.integer(stats::runif(1, 0, 999)))
        self$layout = 'TB'
        self$template = glue(HTML_TEMPLATE,
          code_snippet=WORKFLOW_GRAPH_SCRIPT_TEMPLATE,
          console_snippet='')
      },

      render_html = function(portrait=TRUE){
        if (isFALSE(portrait))
          self$layout = 'LR'
        else
          self$layout = 'TB'
        return(
          glue(self$template,
             element_id=self$element_id,
             definition=self$json_definition,
             layout=self$layout,
             css=CSS_URL,
             jslib=JSLIB_URL,
             graph_legend_template="",
             .open = "$$",
             .close = "$$")
        )
      },

      show = function(portrait=TRUE){
        display_html <- pkg_method("display_html","IRdisplay")
        return(display_html(
          sefl$render_html(portrait)
          )
        )
      }
  ),
  lock_objects=F
)

ExecutionGraphWidget = R6Class("ExecutionGraphWidget",
  public = list(
    initialize = function(json_definition, json_events, execution_arn){
      self$json_definition = json_definition
      self$json_events = json_events
      self$element_id = sprintf('graph-%d', as.integer(stats::runif(1, 0, 999)))
      self$layout = 'TB'
      self$template = glue(HTML_TEMPLATE,
        code_snippet=EXECUTION_GRAPH_SCRIPT_TEMPLATE,
        console_snippet=EXECUTION_URL_TEMPLATE)
      self$console_url = create_sfn_execution_url(execution_arn)
    },

    show = function(portrait=TRUE){
      display_html <- pkg_method("display_html","IRdisplay")

      if (isFALSE(portrait))
        self$layout = 'LR'
      else
        self$layout = 'TB'

      return(display_html(
        glue(self$template,
        element_id=self$element_id,
        definition=self$json_definition,
        events=self$json_events,
        layout=self$layout,
        css=CSS_URL,
        jslib=JSLIB_URL,
        graph_legend_template=EXECUTION_GRAPH_LEGEND_TEMPLATE,
        console=self$console_url,
        .open = "$$",
        .close = "$$")
        )
      )
    }
  ),
  lock_objects=F
)
