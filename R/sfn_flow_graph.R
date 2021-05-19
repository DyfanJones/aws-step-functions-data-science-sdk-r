#' Render AWS Stepfunction Statemachine Graphs
#'
#' Render graphs from definition from \code{Workflow} class.
#'
#' @importFrom htmlwidgets createWidget
#'
#' @export
sfn_flow_graph <- function(definition, element_id, layout,  width = NULL, height = NULL) {

  # forward options using x
  x = list(
    element_id = paste0("#", element_id),
    layout = layout,
    definition = definition,
    height = height
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'sfn_flow_graph',
    x,
    width = width,
    height = height,
    package = 'stepfunctions',
    elementId = element_id
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
