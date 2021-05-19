
#' @importFrom htmlwidgets createWidget shinyWidgetOutput shinyRenderWidget

#' @title Render AWS Stepfunction Statemachine Graphs
#' @description Render graphs from definition from \code{Workflow} class.
#' @param definition (json/list): Amazon States Language \url{https://states-language.net/spec.html}.
#' @param portrait (boolean): Rotation of rendered state machine graph
#' @param width (numeric): Width for graph to render
#' @param height (numeric): Height for graph to render
#' @export
sfn_flow_graph <- function(definition, portrait = FALSE,  width = NULL, height = 600) {

  if (isFALSE(portrait))
    layout = 'LR'
  else
    layout = 'TB'

  # forward options using params
  params = list(
    definition = definition,
    layout = layout
  )
  # create widget
  htmlwidgets::createWidget(
    name = 'sfn_flow_graph',
    params,
    width = width,
    height = height,
    package = 'stepfunctions',
    elementId = sprintf('graph-%d', as.integer(stats::runif(1, 0, 999)))
  )
}

#' Shiny bindings for sfn_flow_graph
#'
#' Output and render functions for using sfn_flow_graph within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a sfn_flow_graph
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name sfn_flow_graph-shiny
#'
#' @export
sfn_flow_graphOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'sfn_flow_graph', width, height, package = 'stepfunctions')
}


#' @rdname sfn_flow_graph-shiny
#' @export
renderSfn_flow_graph <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, sfn_flow_graphOutput, env, quoted = TRUE)
}
