# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/src/stepfunctions/steps/utils.py

tags_dict_to_kv_list = function(tags_dict){
  kv_list = lapply(names(tags_dict), function(k) list("Key"=k, "Value"=tags_dict[[k]]))
  return(kv_list)
}
