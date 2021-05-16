
`%||%` <- function(x, y){if (is.null(x)) return(y) else return(x)}

split_str <- function(str, split = ","){unlist(strsplit(str, split = split))}

Enum <- function(..., .class=NULL) {
  kwargs = list(...)
  env = list2env(kwargs, parent = emptyenv())
  lockEnvironment(env, bindings = TRUE)
  subclass <- Filter(Negate(is.null) ,c(.class, "Enum"))
  class(env) <- c(subclass, class(env))
  return(env)
}

#' @export
print.Enum <- function(env){
  l_env = as.list(env)
  values = paste(names(env), shQuote(unname(l_env)), sep = ": ")
  cat(sprintf("<Enum environment: %s>\n", data.table::address(env)))
  cat("Values:\n")
  cat(paste("  -", values, collapse = "\n"))
}

pkg_method <- function(fun, pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(fun,' requires the ', pkg,' package, please install it first and try again',
         call. = F)}
  fun_name <- utils::getFromNamespace(fun, pkg)
  return(fun_name)
}

