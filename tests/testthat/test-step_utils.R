
aws = c('af-south-1',
        'ap-east-1',
        'ap-northeast-1',
        'ap-northeast-2',
        'ap-northeast-3',
        'ap-south-1',
        'ap-southeast-1',
        'ap-southeast-2',
        'ca-central-1',
        'eu-central-1',
        'eu-north-1',
        'eu-south-1',
        'eu-west-1',
        'eu-west-2',
        'eu-west-3',
        'me-south-1',
        'sa-east-1',
        'us-east-1',
        'us-east-2',
        'us-west-1',
        'us-west-2')
aws_cn = c('cn-north-1', 'cn-northwest-1')
aws_us_gov = c('us-gov-east-1', 'us-gov-west-1')
aws_iso = 'us-iso-east-1'
aws_iso_b = 'us-isob-east-1'

test_that("check if correct partition is returned",{
  Aws = sapply(aws, get_aws_partition)
  AwsCn = sapply(aws_cn, get_aws_partition)
  AwsUsGov = sapply(aws_us_gov, get_aws_partition)
  AwsIso = sapply(aws_iso, get_aws_partition)
  AwsIsoB = sapply(aws_iso_b, get_aws_partition)

  expect_equal(unname(Aws), rep("aws", length(aws)))
  expect_equal(unname(AwsCn), rep("aws-cn", length(aws_cn)))
  expect_equal(unname(AwsUsGov), rep("aws-us-gov", length(aws_us_gov)))
  expect_equal(unname(AwsIso), rep("aws-iso", length(aws_iso)))
  expect_equal(unname(AwsIsoB), rep("aws-iso-b", length(aws_iso_b)))
})
