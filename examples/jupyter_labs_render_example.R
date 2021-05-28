library(httr)
library(glue)
JSLIB_URL = paste0(stepfunctions:::JSLIB_URL,".js")
JS = GET(JSLIB_URL)
JS =rawToChar(httr::content(JS, "raw"))

NOJS_HTML_TEMPLATE = '
<link rel="stylesheet" type="text/css" href="$$css$$">
<div id="$$element_id$$" class="workflowgraph">
    $$graph_legend_template$$
    <svg></svg>
    {console_snippet}
</div>
'
JLAB_WORKFLOW_GRAPH_SCRIPT_TEMPLATE = '
var element = document.getElementById(\'$$element_id$$\')
var options = {
    width: parseFloat(getComputedStyle(element, null).width.replace("px", "")),
    height: 600,
    layout: \'$$layout$$\',
    resizeHeight: true
};
var definition = $$definition$$;
var elementId = \'#$$element_id$$\';
var graph = new sfn.StateMachineGraph(definition, elementId, options);
graph.render();
'

json_definition = workflow$definition$to_json()
element_id = sprintf('graph-%d', as.integer(stats::runif(1, 0, 999)))
layout = 'TB'

template = glue(NOJS_HTML_TEMPLATE,
                code_snippet=JLAB_WORKFLOW_GRAPH_SCRIPT_TEMPLATE,
                console_snippet='')

no_js_hmt = glue(template,
                 element_id=element_id,
                 definition=json_definition,
                 layout=layout,
                 css=stepfunctions:::CSS_URL,
                 jslib=JSLIB_URL,
                 graph_legend_template="",
                 .open = "$$",
                 .close = "$$"
)

jlab_graph = glue(JLAB_WORKFLOW_GRAPH_SCRIPT_TEMPLATE,
                  element_id=element_id,
                  definition=json_definition,
                  layout=layout,
                  .open = "$$",
                  .close = "$$"
)
