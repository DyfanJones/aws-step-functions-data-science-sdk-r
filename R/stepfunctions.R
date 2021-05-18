#' stepfunction: this is just a placeholder
#'
#' @import R6
#' @importFrom paws sfn
#' @importFrom jsonlite toJSON prettify
#' @importFrom yaml as.yaml
#' @import lgr
#' @importFrom glue glue
"_PACKAGE"

pkg_env = new.env(emptyenv())

.onLoad <- function(libname, pkgname) {
  # set package logs and don't propagate root logs
  .logger = lgr::get_logger(name = pkgname)$set_propagate(FALSE)

  # set logging layout
  .logger$add_appender(
    lgr::AppenderConsole$new(
      layout=log_layout()
    )
  )

  # set package logger
  assign(
    "LOGGER",
    .logger,
    envir = parent.env(environment())
  )

  pkg_env$jupyter <- jupyter_check()
}

jupyter_check <- function(){
  j_pid = Sys.getenv("JPY_PARENT_PID")
  if(nzchar(j_pid)) TRUE else FALSE
}

# set format for SageMaker
log_layout <- function(
  log_fmt="%L[%t] %m",
  timestamp_fmt = "%Y-%m-%d %H:%M:%OS",
  log_cols = log_colours()) {
  lgr::LayoutFormat$new(
    fmt = log_fmt,
    timestamp_fmt = timestamp_fmt,
    colors = log_cols
  )
}

# utilise lgr colour scheme https://github.com/s-fleck/lgr/blob/master/R/lgr-package.R#L66-L72
# This is so that it can be changed (if needed) in the future
log_colours <- function(error="#BB3333",
                        warn="#EEBB50",
                        debug="#808080",
                        trace="#808080"){
  log_col <- list()
  if (requireNamespace("crayon", quietly = TRUE) && crayon::has_color()){
    style_error <- crayon::make_style(error, colors = 256)
    style_fatal <- function(...) style_error(crayon::bold(...))
    style_warning <- crayon::make_style(warn, colors = 256)
    style_debug <- crayon::make_style(debug, grey = TRUE)
    style_trace <- crayon::make_style(trace, grey = TRUE)
    log_col[["fatal"]] <- style_fatal
    log_col[["error"]] <- style_error
    log_col[["warn"]] <- style_warning
    log_col[["debug"]] <- style_debug
    log_col[["trace"]] <- style_trace
  }
  return(log_col)
}
