# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/src/stepfunctions/steps/utils.py

tags_dict_to_kv_list = function(tags_dict){
  kv_list = lapply(names(tags_dict), function(k) list("Key"=k, "Value"=tags_dict[[k]]))
  return(kv_list)
}

get_aws_partition = function(region){
  partitions = list(
    'aws-cn'='cn-*',
    'aws-us-gov'='us-gov-*',
    'aws-iso'='us-iso-*',
    'aws-iso-b'='us-isob-*',
    'aws'= "*"
  )
  matches = partitions[sapply(partitions, function(x) grepl(x, region))]
  matches = matches[order(nchar(matches), decreasing = TRUE)][1]
  return(names(matches))
}
