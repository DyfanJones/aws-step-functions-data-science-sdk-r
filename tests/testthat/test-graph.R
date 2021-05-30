# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/tests/unit/test_graph.py


test_that("test nested parallel example", {
  nested_level_1 = Parallel$new('NestedStateLevel1')
  nested_level_1$add_branch(Succeed$new('NestedStateLevel2'))

  first_state = Parallel$new('FirstState')
  first_state$add_branch(nested_level_1)
  result = Graph$new(first_state, comment='This is a test.', version='1.0', timeout_seconds=3600)
  expect_equal(
    result$to_list(),
    list(
      'Comment'='This is a test.',
      'Version'='1.0',
      'TimeoutSeconds'=3600,
      'StartAt'='FirstState',
      'States'=list(
        'FirstState'=list(
          'Type'='Parallel',
          'End'=TRUE,
          'Branches'=list(
            list(
              'StartAt'='NestedStateLevel1',
              'States'=list(
                'NestedStateLevel1'=list(
                  'Type'='Parallel',
                  'End'=TRUE,
                  'Branches'=list(
                    list(
                      'StartAt'='NestedStateLevel2',
                      'States'=list(
                        'NestedStateLevel2'=list(
                          'Type'='Succeed')
                        )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
})

test_that("test wait loop",{
  choice_rule = ChoiceRule$new()
  first_state = Task$new('FirstState', resource='arn:aws:lambda:us-east-1:1234567890:function:FirstState')
  retry = Chain$new(list(Pass$new('Retry'), Pass$new('Cleanup'), first_state))

  choice_state = Choice$new('Is Completed?')
  choice_state$add_choice(choice_rule$BooleanEquals('$.Completed', TRUE), Succeed$new('Complete'))
  choice_state$add_choice(choice_rule$BooleanEquals('$.Completed', FALSE), retry)
  first_state$.next(choice_state)

  result = Graph$new(first_state)
  expect_equal(
    result$to_list(),
    list(
      "StartAt"="FirstState",
      'States'=list(
        'FirstState'=list(
          'Resource'='arn:aws:lambda:us-east-1:1234567890:function:FirstState',
          'Type'='Task',
          'Next'='Is Completed?'),
        'Is Completed?'=list(
          'Type'='Choice',
          'Choices'=list(
            list(
              'Variable'='$.Completed',
              'BooleanEquals'=TRUE,
              'Next'='Complete'),
            list(
              'Variable'='$.Completed',
              'BooleanEquals'=FALSE,
              'Next'='Retry')
            )
        ),
        'Complete'=list(
          'Type'='Succeed'),
        'Retry'=list(
          'Type'='Pass',
          'Next'='Cleanup'),
        'Cleanup'=list(
          'Type'='Pass',
          'Next'='FirstState')
      )
    )
  )
})

test_that("test wait example", {
  chain = Chain$new()
  chain$append(Task$new('FirstState', resource='arn:aws:lambda:us-east-1:1234567890:function:StartState'))
  chain$append(Wait$new('wait_using_seconds', seconds=10))
  chain$append(Wait$new('wait_using_timestamp', timestamp='2015-09-04T01:59:00Z'))
  chain$append(Wait$new('wait_using_timestamp_path', timestamp_path='$.expirydate'))
  chain$append(Wait$new('wait_using_seconds_path', seconds_path='$.expiryseconds'))
  chain$append(Task$new('FinalState', resource='arn:aws:lambda:us-east-1:1234567890:function:EndLambda'))

  result = Graph$new(chain)

  expect_equal(
    result$to_list(),
    list(
      'StartAt'='FirstState',
      'States'=list(
        'FirstState'=list(
          'Resource'='arn:aws:lambda:us-east-1:1234567890:function:StartState',
          'Type'='Task',
          'Next'='wait_using_seconds'),
        'wait_using_seconds'=list(
          'Seconds'=10,
          'Type'='Wait',
          'Next'='wait_using_timestamp'),
        'wait_using_timestamp'=list(
          'Timestamp'='2015-09-04T01:59:00Z',
          'Type'='Wait',
          'Next'='wait_using_timestamp_path'),
        'wait_using_timestamp_path'=list(
          'TimestampPath'='$.expirydate',
          'Type'='Wait',
          'Next'='wait_using_seconds_path'),
        'wait_using_seconds_path'=list(
          'SecondsPath'='$.expiryseconds',
          'Type'='Wait',
          'Next'='FinalState'),
        'FinalState'=list(
          'Resource'='arn:aws:lambda:us-east-1:1234567890:function:EndLambda',
          'Type'='Task',
          'End'=TRUE)
      )
    )
  )
})

test_that("test choice example",{
  next_state = Task$new('NextState', resource='arn:aws:lambda:us-east-1:1234567890:function:NextState')

  choice_rule = ChoiceRule$new()
  choice_state = Choice$new('ChoiceState')
  choice_state$default_choice(Fail$new('DefaultState', error='DefaultStateError', cause='No Matches!'))
  choice_state$add_choice(choice_rule$NumericEquals(variable='$.foo', value=1), Chain$new(list(
    Task$new('FirstMatchState', resource='arn:aws:lambda:us-east-1:1234567890:function:FirstMatchState'),
    next_state)
  ))

  choice_state$add_choice(choice_rule$NumericEquals(variable='$.foo', value=2), Chain$new(list(
    Task$new('SecondMatchState', resource='arn:aws:lambda:us-east-1:1234567890:function:SecondMatchState'),
    next_state)
  ))

  chain = Chain$new()
  chain$append(Task$new('FirstState', resource='arn:aws:lambda:us-east-1:1234567890:function:StartLambda'))
  chain$append(choice_state)

  result = Graph$new(chain)

  expect_equal(
    result$to_list(),
    list(
      'StartAt'='FirstState',
      'States'=list(
        'FirstState'=list(
          'Resource'='arn:aws:lambda:us-east-1:1234567890:function:StartLambda',
          'Type'='Task',
          'Next'='ChoiceState'),
        'ChoiceState'=list(
          'Type'='Choice',
          'Choices'=list(
            list(
              'Variable'='$.foo',
              'NumericEquals'=1,
              'Next'='FirstMatchState'),
            list(
              'Variable'='$.foo',
              'NumericEquals'=2,
              'Next'='SecondMatchState')
            ),
          'Default'='DefaultState'
          ),
        'DefaultState'=list(
          'Error'='DefaultStateError',
          'Cause'='No Matches!',
          'Type'='Fail'),
        'FirstMatchState'=list(
          'Resource'='arn:aws:lambda:us-east-1:1234567890:function:FirstMatchState',
          'Type'='Task',
          'Next'='NextState'),
        'NextState'=list(
          'Resource'='arn:aws:lambda:us-east-1:1234567890:function:NextState',
          'Type'='Task',
          'End'=TRUE),
        'SecondMatchState'=list(
          'Resource'='arn:aws:lambda:us-east-1:1234567890:function:SecondMatchState',
          'Type'='Task',
          'Next'='NextState')
      )
    )
  )

})

test_that("test graph from string",{
  g = Graph$new(Chain$new(list(Pass$new('HelloWorld'))))
  g1 = FrozenGraph$public_methods$from_json(g$to_json())
  expect_true(inherits(g1, "Graph"))
  expect_equal(g$to_list(), g1$to_list())
})
