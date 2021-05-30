# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/src/stepfunctions/inputs/placeholders.py

#' @include inputs_utils.R

#' @import R6
#' @importFrom jsonlite toJSON

#' @title Placeholder Class
#' @description A collection of Placeholder variables.
#' @keywords internal
#' @export
Placeholder = R6Class("Placeholder",
  public = list(

    #' @description Initialize Placeholder class
    #' @param schema (dict, optional): Schema for the placeholder collection. (default: None)
    #'              Example below::
    #'              \code{
    #'              list(
    #'                 'ModelName'=str,
    #'                 'JobName'=str,
    #'                 'Hyperparameters'= list(
    #'                    'tol'=float)
    #'                 )
    #'              }
    #'              Keyword Args:
    #' @param name (str, optional): Name of the placeholder variable. (default: None)
    #' @param type (type, optional): Type of the placeholder variable. (default: None)
    #' @param parent (Placeholder, optional): Parent variable for a placeholder variable. (default: None)
    initialize = function(schema=NULL,
                          name = NULL,
                          type = NULL,
                          parent = NULL){
      self$store = list()
      self$immutable = FALSE
      self$schema = schema
      if (length(self$schema)>0)
        private$.set_schema(schema)
      private$.make_immutable()
      self$json_str_template = "%s"

      self$name = name
      self$type = type
      self$parent = parent
    },

    #' @description Create a placeholder variable with an associated type.
    #' @param name (str): Name of the placeholder variable.
    #' @param type (type): Type of the placeholder variable.
    #' @return Placeholder: Placeholder variable.
    get = function(name, type){
      if (!private$.is_valid_name(name))
        stop('Key name can only be string or integer')
      if (name %in% names(self$store)){
        curr_variable = self$store[[name]]
        if (curr_variable$type != type)
          stop(sprintf(
            'Key already exists with a different value type: %s',
            curr_variable$type))
        return(curr_variable)
      } else {
        self$store[[name]] = private$.create_variable(name=name, parent=self, type=type)
        return(self$store[[name]])
      }
    },

    #' @description Generate a schema for the placeholder collection as a R named list.
    #' @return list: Placeholder collection schema.
    get_schema_as_list = function(){
      schema = list()
      for (k in names(self$store)){
        v = self$store[[k]]
        if (v$.__enclos_env__$private$.is_empty()){
          schema[[k]] = v$type %||% character(0)
        } else {
          schema[[k]] = v$get_schema_as_list()
        }
      }
      return(schema)
    },

    #' @description Generate a schema for the placeholder collection as a JSON formatted string.
    #' @param pretty (bool, optional): Boolean flag set to `True` if JSON string should be prettified. `False`, otherwise. (default: False)
    #' @return str: JSON formatted string representation of the block.
    get_schema_as_json = function(pretty = FALSE){
      dict_schema_str = replace_type_with_str(self$get_schema_as_dict())
      return(toJSON(dict_schema_str, pretty = pretty, auto_unbox = TRUE))
    },

    #' @description Check if the placeholder collection contains the specified placeholder variable.
    #' @param placeholder (Placeholder): Placeholder variable to search for, in the collection.
    #' @return bool: `True` if placeholder variable was found in the collection. `False`, otherwise.
    contains = function(placeholder){
      for (k in names(self$store)){
        v = self$store[[k]]
        if(placeholder == v)
          return(TRUE)
        else if (v$contains(placeholder))
          return(TRUE)
      }
      return(FALSE)
    },

    #' @description Validate a specified input against the placeholder collection schema.
    #' @param input (list): Input to validate against the placeholder collection schema.
    #' @return ValidationResult: Named tuple with the keys:
    #'     \itemize{
    #'         \item{`valid` (Boolean): Representing the result of validation}
    #'         \item{`keys_missing` (list(str)): List of keys missing in the input}
    #'         \item{`keys_type_mismatch` (list(str), type, type): List of tuples with key name, expected type, and provided type.}
    #'      }
    validate = function(input=NULL){
      input = as.list(input)
      if (is.null(input))
        return(list(valid=FALSE, keys_missing=NULL, keys_type_mismatch=NULL))
      flattened_schema = flatten(self$get_schema_as_list())
      flattened_input = flatten(input)
      keys_missing = flattened_schema[(names(flattened_input) %in% flattened_schema)]
      keys_type_mismatch = list()
      for (k in names(flattened_input)){
        v = flattened_input[[k]]
        if (k %in% names(flattened_schema) && !inherits(v, flattened_schema[[k]]))
          keys_type_mismatch = c(keys_type_mismatch, list(k, flattened_schema[[k]], class(v)))
      }
      if (length(keys_missing) > 0 || length(keys_type_mismatch) > 0){
        valid = FALSE
      } else {
        valid = TRUE}
      return(list(valid=valid, keys_missing=keys_missing, keys_type_mismatch=keys_type_mismatch))
    },

    #' @description Returns a JSON path representation of the placeholder variable to be used for step parameters.
    #' @return str: JSON path representation of the placeholder variable
    to_jsonpath = function(){
      return(sprintf(self$json_str_template, private$.join_path(private$.get_path())))
    }
  ),

  private = list(
    .create_variable = function(name, parent, type=NULL){
      stop("NotImplementedError")
    },

    # Get path to a placeholder variable node in the collection.
    .get_path = function(){
      path = list()
      node = self
      while (!is.null(node$name)){
        path = c(path, node$name)
        node = node$parent
      }
      return(rev(path))
    },

    # Check if the store for a placeholder collection/variable is empty.
    .empty_path = function(){
      return(length(self$store) == 0)
    },

    # Set the schema for a placeholder collection.
    .set_schema = function(schema, path = c()){

      for (k in names(schema)){
        v = schema[[k]]
        if(inherits(v, "list")){
          self$.set_schema(v, c(path, k))
        } else {
          current = self
          for (node in path){
            current = current$get(node,list())}
          temp = current$get(k, v)}
      }
    },

    # Make a placeholder collection (including all variables contained) immutable.
    .make_immutable = function(){
      for (k in names(self$store)){
        v = self$store[[k]]
        if (inherits(v, "Placeholder"))
          v$.__enclos_env__$private$.make_immutable()
      }
      self$immutable = TRUE
    },

    .is_valid_name = function(name){
      if(is.character(name) || is.integer(name))
        return(TRUE)
      else
        return(FALSE)
    },

    .join_path = function(path){
      subscript_list = list()
      for (i in path){
        if (is.character(i)){
          subscript_list = c(subscript_list, sprintf("['%s']",i))
        } else if (is.integer(i))
          subscript_list = c(subscript_list, sprintf('[%s]',i))
      }
      return (paste0(subscript_list, collapse = ""))
    }
  ),
  lock_objects = F
)

#' @title ExecutionInput Class
#' @description Top-level class for execution input placeholders.
#' @export
ExecutionInput = R6Class("ExecutionInput",
  inherit = Placeholder,
  public = list(

    #' @description Initialize Placeholder class
    #' @param schema (dict, optional): Schema for the placeholder collection. (default: None)
    #' @param ... : other parameters from Placeholder
    initialize = function(schema=NULL, ...){
      super$initialize(schema, ...)
      self$json_str_template = '$$.Execution.Input%s'
    }
  ),
  private = list(

    # Creates a placeholder variable for Workflow Input.
    # A placeholder variable can only be created if the collection is not immutable due to a pre-specified schema.
    .create_variable = function(name, parent, type=NULL){
      if (self$immutable)
        stop("Placeholder variable does not conform to schema set for the placeholder collection.", call. = F)
      if (!is.null(type))
        return(ExecutionInput$new(name=name, parent=parent, type=type))
      else
        return(ExecutionInput(name=name, parent=parent))
    }
  ),
  lock_object=F
)

#' @title StepInput Class
#' @description Top-level class for step input placeholders.
#' @export
StepInput = R6Class(
  inherit = Placeholder,
  public = list(

    #' @description Initialize Placeholder class
    #' @param schema (dict, optional): Schema for the placeholder collection. (default: None)
    #' @param ... : other parameters from Placeholder
    initialize = function(schema=NULL, ...){
      super$initialize(schema, ...)
      self$json_str_template = '$%s'
    }
  ),
  private = list(

    # Creates a placeholder variable for Step Input.
    # A placeholder variable can only be created if the collection is not immutable due to a pre-specified schema.
    .create_variable = function(name, parent, type=NULL){
      if (self.immutable)
        stop("Placeholder variable does not conform to schema set for the placeholder collection.", call.=F)
      if (!is.null(type))
        return(StepInput$new(name=name, parent=parent, type=type))
      else
        return(StepInput$new(name=name, parent=parent))
    }
  ),
  lock_object = F
)
