
`%||%` <- function(x, y){if (is.null(x)) return(y) else return(x)}

split_str <- function(str, split = ","){unlist(strsplit(str, split = split))}

is_list_named = function(x){
  inherits(x, "list") && length(names(x)) > 0
}

Enum <- function(..., .class=NULL) {
  kwargs = list(...)
  env = list2env(kwargs, parent = emptyenv())
  lockEnvironment(env, bindings = TRUE)
  subclass <- Filter(Negate(is.null) ,c(.class, "Enum"))
  class(env) <- c(subclass, class(env))
  return(env)
}

#' @export
print.Enum <- function(x, ...){
  l_env = as.list(x)
  values = paste(names(x), shQuote(unname(l_env)), sep = ": ")
  cat("<Enum environment>\n")
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

get_region <- pkg_method("get_region", "paws.common")

islistempty = function(obj) {(is.null(obj) || length(obj) == 0)}
