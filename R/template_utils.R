# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/src/stepfunctions/template/utils.py

# Replace the parameters using $$.Execution.Input.
replace_parameters_with_context_object = function(step){
  updated_parameters = list()
  for (k in names(step$parameters)){
    updated_parameters[sprintf('%s.$',k)] = sprintf("$$.Execution.Input['%s'].%s", step$state_id, k)
  }
  return(updated_parameters)
}

replace_parameters_with_jsonpath = function(step, params){

  replace_values = function(src_params, dest_params){
    if (inherits(dest_params, "list")){
      for (key in names(dest_params)){
        if (endsWith(key, '$')){
          original_key = substring(key,1, nhcar(key)-2) # Remove .$ in the end
          src_params[[original_key]] = NULL
          src_params[[key]] = dest_params[[key]]
        } else
        scr_params = replace_values(src_params[[key]], dest_params[[key]])
        }
      }
      return(src_params)
    }

  task_parameters = step$parameters
  task_parameters = replace_values(task_parameters, params)
  return(task_parameters)
}
