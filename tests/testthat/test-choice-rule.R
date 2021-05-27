# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/tests/unit/test_choice_rule.py


test_that("test variable must start with prefix", {
  expect_error(
    ChoiceRule$new()$StringEquals('Variable', '42'))
})

test_that("test variable value must be consistent", {
  choice_rule = ChoiceRule$new()
  string_functions = c(
    'StringEquals',
    'StringLessThan',
    'StringGreaterThan',
    'StringLessThanEquals',
    'StringGreaterThanEquals')
  for(str_fun in string_functions){
    expect_error(
      choice_rule[[str_fun]]('$.Variable', 42)
    )
  }

  numeric_functions = c(
    'NumericEquals',
    'NumericLessThan',
    'NumericGreaterThan',
    'NumericLessThanEquals',
    'NumericGreaterThanEquals')
  for (num_fun in numeric_functions){
    expect_error(
      choice_rule[[num_fun]]('$.Variable', "ABC")
    )
  }
  expect_error(
    choice_rule$BooleanEquals('$.Variable', 42)
  )

  timestamp_functions = c(
    'TimestampEquals',
    'TimestampLessThan',
    'TimestampGreaterThan',
    'TimestampLessThanEquals',
    'TimestampGreaterThanEquals')

  for (tms_fun in timestamp_functions){
    expect_error(
      choice_rule[[tms_fun]]('$.Variable', TRUE))
  }
})

test_that("test path comparator raises error when value is not a path", {
  choice_rule = ChoiceRule$new()
  path_comparators = c(
    'StringEqualsPath',
    'NumericEqualsPath',
    'TimestampEqualsPath',
    'BooleanEqualsPath')

  for(path_comp in path_comparators){
    expect_error(
    choice_rule[[path_comp]]('$.Variable', 'string'))
  }
})

test_that("test is comparator raises error when value is not a bool", {
  choice_rule = ChoiceRule$new()
  type_comparators = c(
    'IsPresent',
    'IsNull',
    'IsString',
    'IsNumeric',
    'IsBoolean',
    'IsTimestamp')

  for(type_comp in type_comparators){
    expect_error(
      choice_rule[[path_comp]]('$.Variable', 'string'))
    expect_error(
      choice_rule[[path_comp]]('$.Variable', 101))
  }
})


test_that("test static comparator serialization", {
  choice_rule = ChoiceRule$new()
  string_timestamp_static_comparators = c(
    'StringEquals',
    'StringLessThan',
    'StringLessThanEquals',
    'StringGreaterThan',
    'StringGreaterThanEquals',
    'TimestampEquals',
    'TimestampLessThan',
    'TimestampGreaterThan',
    'TimestampLessThanEquals')
  for(str_tms_static_comp in string_timestamp_static_comparators){
    type_rule = choice_rule[[str_tms_static_comp]]('$.input', 'hello')
    expected_list = list('Variable'='$.input')
    expected_list[[str_tms_static_comp]] = 'hello'
    expect_equal(type_rule$to_list(), expected_list)
  }

  number_static_comparators = c(
    'NumericEquals',
    'NumericLessThan',
    'NumericGreaterThan',
    'NumericLessThanEquals',
    'NumericGreaterThanEquals')
  for(nm_static_comp in number_static_comparators){
    type_rule = choice_rule[[nm_static_comp]]('$.input', 123)
    expected_list = list('Variable'='$.input')
    expected_list[[nm_static_comp]] = 123
    expect_equal(type_rule$to_list(), expected_list)
  }

  boolean_static_comparators = c(
    "BooleanEquals"
  )

  for (bl_static_comp in boolean_static_comparators){
    type_rule = choice_rule[[bl_static_comp]]('$.input', FALSE)
    expected_list = list('Variable'='$.input')
    expected_list[[bl_static_comp]] = FALSE
    expect_equal(type_rule$to_list(), expected_list)
  }
})

test_that("test dynamic comparator serialization",{
  choice_rule = ChoiceRule$new()
  dynamic_comparators = c(
    'StringEqualsPath',
    'StringLessThanPath',
    'StringLessThanEqualsPath',
    'StringGreaterThanPath',
    'StringGreaterThanEqualsPath',
    'TimestampEqualsPath',
    'TimestampLessThanPath',
    'TimestampGreaterThanPath',
    'TimestampLessThanEqualsPath',
    'NumericEqualsPath',
    'NumericLessThanPath',
    'NumericGreaterThanPath',
    'NumericLessThanEqualsPath',
    'NumericGreaterThanEqualsPath',
    'BooleanEqualsPath')
  for (dynamic_comparator in dynamic_comparators){
    type_rule = choice_rule[[dynamic_comparator]]('$.input', '$.input2')
    expected_list = list('Variable'='$.input')
    expected_list[[dynamic_comparator]] = '$.input2'
    expect_equal(type_rule$to_list(), expected_list)
  }
})

test_that("test type check comparators serialization",{
  choice_rule = ChoiceRule$new()
  type_comparators = c(
    'IsPresent',
    'IsNull',
    'IsString',
    'IsNumeric',
    'IsBoolean',
    'IsTimestamp')
  for (type_comparator in type_comparators){
    type_rule = choice_rule[[type_comparator]]('$.input', TRUE)
    expected_list = list('Variable'='$.input')
    expected_list[[type_comparator]] = TRUE
    expect_equal(type_rule$to_list(), expected_list)
  }
})

test_that("test string matches serialization",{
  choice_rule = ChoiceRule$new()

  string_matches_rule = choice_rule$StringMatches('$.input', 'hello*world\\*')

  expect_equal(
    string_matches_rule$to_list(),
    list('Variable'='$.input',
    'StringMatches'='hello*world\\*')
  )
})


test_that("test rule serialization", {
  choice_rule = ChoiceRule$new()

  bool_rule = choice_rule$BooleanEquals('$.BooleanVariable', TRUE)
  expect_equal(bool_rule$to_list(), list(
    'Variable'='$.BooleanVariable',
    'BooleanEquals'=TRUE
  ))

  string_rule = choice_rule$StringEquals('$.StringVariable', 'ABC')
  expect_equal(string_rule$to_list(), list(
    'Variable'='$.StringVariable',
    'StringEquals'='ABC'
  ))

  and_rule = choice_rule$And(c(bool_rule, string_rule))
  expect_equal(and_rule$to_list(),list(
    'And'=list(
      list(
        'Variable'='$.BooleanVariable',
        'BooleanEquals'=TRUE),
      list(
        'Variable'='$.StringVariable',
        'StringEquals'='ABC')
      )
    )
  )

  not_rule = choice_rule$Not(string_rule)
  expect_equal(not_rule$to_list(),list(
    'Not'=list(
      'Variable'='$.StringVariable',
      'StringEquals'="ABC")
    )
  )

  compound_rule = choice_rule$Or(c(and_rule, not_rule))
  expect_equal(compound_rule$to_list(),list(
    'Or'=list(
      list(
        'And'=list(list(
          'Variable'='$.BooleanVariable',
          'BooleanEquals'=TRUE),
          list(
          'Variable'='$.StringVariable',
          'StringEquals'='ABC')
        )
      ),
      list(
        'Not'=list(
          'Variable'='$.StringVariable',
          'StringEquals'='ABC')
        )
      )
    )
  )
})

