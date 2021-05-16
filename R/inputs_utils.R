# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/src/stepfunctions/inputs/utils.py

flatten <- function(input, parent_key='', sep='.'){
  items = list()

  for(i in seq_along(input)){
    k = names(input)[i]
    v = input[[i]]
    if(nchar(parent_key) > 0) {
      flattened_key = paste0(parent_key, sep, k)
    } else {
      flattened_key = k
      if (inherits(v, "list")){
        ll = flatten(v, flattened_key, sep=sep)
        items= c(items, names(ll), unname(ll))
      } else {
        items = c(items, flattened_key, v)
      }
    }
  }
  return(list(items))
}

replace_type_with_str = function(schema){
  schema_with_str = {}
  for (i in seq_along(schema)){
    k = names(schema)[i]
    v = schema[[i]]
    if (inherits(v, "list")) {
      schema_with_str[[k]] = replace_type_with_str(v)
    } else {
      schema_with_str[[k]] = names(v)
    }
  }
  return(schema_with_str)
}
