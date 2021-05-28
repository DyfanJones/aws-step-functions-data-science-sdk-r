# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/tests/unit/test_steps.py

library(lgr)

LOGGER = get_logger("stepfunctions")

test_that("test to pascalcase",{
  expect_equal('InputPath', to_pascalcase('input_path'))
})

test_that("test state creation", {
  state = State$new(
    state_id='StartState',
    state_type='Void',
    comment='This is a comment',
    input_path='$.Input',
    output_path='$.Output',
    parameters=list('Key'='Value'),
    result_path='$.Result'
  )
  expect_list = list(
    'Type'='Void',
    'Comment'='This is a comment',
    'InputPath'='$.Input',
    'OutputPath'='$.Output',
    'Parameters'=list('Key'='Value'),
    'ResultPath'='$.Result',
    'End'=TRUE
  )
  expect_equal(
    state$to_list()[names(expect_list)],
    expect_list
  )

  expect_error(
    State$new(state_id='State', unknown_attribute=TRUE)
  )
})

test_that("test pass state creation", {
  pass_state = Pass$new('Pass', result='Pass')
  expect_equal(pass_state$state_id, 'Pass')
  expect_list = list(
    'Type'='Pass',
    'Result'='Pass',
    'End'=TRUE
  )
  expect_equal(
    pass_state$to_list()[names(expect_list)],
    expect_list
  )
})

test_that("test verify pass state fields", {
  pass_state = Pass$new(
    state_id='Pass',
    comment='This is a comment',
    parameters=list(),
    input_path='$.InputPath',
    result_path='$.ResultPath',
    result=list()
  )

  expect_equal(pass_state$state_id, 'Pass')
  expect_equal(pass_state$comment, 'This is a comment')
  expect_equal(pass_state$fields$parameters, list())
  expect_equal(pass_state$fields$input_path, '$.InputPath')
  expect_equal(pass_state$fields$result_path, '$.ResultPath')
  expect_equal(pass_state$fields$result , list())

  expect_error(
    Pass$new('Pass', unknown_field='Unknown Field')
  )
})

test_that("test succeed state creation", {
  succeed_state = Succeed$new(
    state_id='Succeed',
    comment='This is a comment'
  )
  expect_equal(succeed_state$state_id, 'Succeed')
  expect_equal(succeed_state$comment, 'This is a comment')
  expect_list = list(
    'Type'='Succeed',
    'Comment'='This is a comment'
  )
  expect_equal(
    succeed_state$to_list()[names(expect_list)],
    expect_list
  )
})

test_that("test verify succeed state fields", {
  expect_error(
    Succeed$new('Succeed', unknown_field='Unknown Field')
  )
})

test_that("test fail creation", {
  fail_state = Fail$new(
    state_id='Fail',
    error='ErrorA',
    cause='Kaiju attack',
    comment='This is a comment'
  )
  expect_equal(fail_state$state_id, 'Fail')
  expect_equal(fail_state$error, 'ErrorA')
  expect_equal(fail_state$cause, 'Kaiju attack')
  expect_equal(fail_state$comment, 'This is a comment')
  expect_list = list(
    'Type'='Fail',
    'Comment'='This is a comment',
    'Error'='ErrorA',
    'Cause'='Kaiju attack'
  )
  expect_equal(
    fail_state$to_list()[names(expect_list)],
    expect_list
  )
})

test_that("test verify fail state fields", {
  expect_error(
    Fail$new('Succeed', unknown_field='Unknown Field')
  )
})

test_that("test wait state creation", {
  wait_state = Wait$new(
    state_id='Wait',
    seconds=10
  )
  expect_equal(wait_state$state_id, 'Wait')
  expect_equal(wait_state$seconds, 10)
  expect_equal(
    wait_state$to_list(),
    list(
    'Seconds'=10,
    'Type'='Wait',
    'End'=TRUE
    )
  )

  wait_state = Wait$new(
    state_id='Wait',
    seconds_path='$.SecondsPath'
  )
  expect_equal(wait_state$state_id, 'Wait')
  expect_equal(wait_state$seconds_path, '$.SecondsPath')
  expect_equal(
    wait_state$to_list(),
    list(
    'SecondsPath'='$.SecondsPath',
    'Type'='Wait',
    'End'=TRUE
    )
  )
})

test_that("test verify wait state fields", {
  expect_error(
    Wait$new(
      state_id='Wait',
      seconds=10,
      seconds_path='$.SecondsPath'
    )
  )
})

test_that("test choice state creation", {
  choise_rule = ChoiceRule$new()
  choice_state = Choice$new('Choice', input_path='$.Input')
  choice_state$add_choice(choise_rule$IsPresent("$.StringVariable1", TRUE), Pass$new("End State 1"))
  choice_state$add_choice(choise_rule$StringEquals("$.StringVariable1", "ABC"), Pass$new("End State 1"))
  choice_state$add_choice(choise_rule$StringLessThanEqualsPath("$.StringVariable2", "$.value"), Pass$new("End State 2"))
  choice_state$default_choice(Pass$new('End State 3'))
  expect_equal(choice_state$state_id, 'Choice')
  expect_equal(length(choice_state$choices), 3)
  expect_equal(choice_state$default$state_id, 'End State 3')
  expect_equal(
    choice_state$to_list(),
    list(
      'InputPath'='$.Input',
      'Type'='Choice',
      'Choices'=list(
        list(
          'Variable'='$.StringVariable1',
          'IsPresent'=TRUE,
          'Next'='End State 1'),
        list(
          'Variable'='$.StringVariable1',
          'StringEquals'='ABC',
          'Next'='End State 1'),
        list(
          'Variable'='$.StringVariable2',
          'StringLessThanEqualsPath'='$.value',
          'Next'='End State 2')
      ),
      'Default'='End State 3'
    )
  )

  expect_error(
    Choice$new('Choice', unknown_field='Unknown Field')
  )
})

test_that("test task state creation", {
  task_state = Task$new('Task', resource='arn:aws:lambda:us-east-1:1234567890:function:StartLambda')
  task_state$add_retry(Retry$new(error_equals=c('ErrorA', 'ErrorB'), interval_seconds=1, max_attempts=2, backoff_rate=2))
  task_state$add_retry(Retry$new(error_equals='ErrorC', interval_seconds=5))
  task_state$add_catch(Catch$new(error_equals='States.ALL', next_step=Pass$new('End State')))
  expect_equal(task_state$type, 'Task')
  expect_equal(length(task_state$retries), 2)
  expect_equal(length(task_state$catches), 1)
  expect_equal(
    task_state$to_list(),
    list(
      'Resource'='arn:aws:lambda:us-east-1:1234567890:function:StartLambda',
      'Type'='Task',
      'End'=TRUE,
      'Retry'=list(
        list(
          'ErrorEquals'=list('ErrorA', 'ErrorB'),
          'IntervalSeconds'=1,
          'MaxAttempts'=2,
          'BackoffRate'=2),
        list(
          'ErrorEquals'=list('ErrorC'),
          'IntervalSeconds'=5)
      ),
      'Catch'=list(
        list(
          'ErrorEquals'=list('States.ALL'),
          'Next'='End State')
      )
    )
  )
})

test_that("test task state creation with dynamic timeout", {
  task_state = Task$new(
    'Task',
    resource='arn:aws:lambda:us-east-1:1234567890:function:StartLambda',
    timeout_seconds_path='$.timeout',
    heartbeat_seconds_path='$.heartbeat'
  )
  expect_equal(
    task_state$to_list(),
    list(
      'Resource'='arn:aws:lambda:us-east-1:1234567890:function:StartLambda',
      'TimeoutSecondsPath'='$.timeout',
      'HeartbeatSecondsPath'='$.heartbeat',
      'Type'='Task',
      'End'=TRUE
    )
  )
})

test_that("test task state create fail for duplicated dynamic timeout fields", {
  expect_error(
    Task$new(
      'Task',
      resource='arn:aws:lambda:us-east-1:1234567890:function:StartLambda',
      timeout_seconds=1,
      timeout_seconds_path='$.timeout'
    )
  )

  expect_error(
    Task$new(
      'Task',
      resource='arn:aws:lambda:us-east-1:1234567890:function:StartLambda',
      heartbeat_seconds=1,
      heartbeat_seconds_path='$.heartbeat'
    )
  )
})

test_that("test parallel state creation", {
  parallel_state = Parallel$new('Parallel')
  parallel_state$add_branch(Pass$new('Branch 1'))
  parallel_state$add_branch(Pass$new('Branch 2'))
  parallel_state$add_branch(Pass$new('Branch 3'))
  expect_equal(parallel_state$type, 'Parallel')
  expect_equal(length(parallel_state$branches), 3)
  expect_equal(
    parallel_state$to_list(),
    list(
      'Type'='Parallel',
      'End'=TRUE,
      'Branches'=list(
        list(
          'StartAt'='Branch 1',
          'States'=list(
            'Branch 1'=list(
              'Type'='Pass',
              'End'=TRUE
            )
          )
        ),
        list(
          'StartAt'='Branch 2',
          'States'=list(
            'Branch 2'=list(
              'Type'='Pass',
              'End'=TRUE
            )
          )
        ),
        list(
          'StartAt'='Branch 3',
          'States'=list(
            'Branch 3'=list(
              'Type'='Pass',
              'End'=TRUE
            )
          )
        )
      )
    )
  )
})

test_that("test map state creation", {
  map_state = Map$new('Map', iterator=Pass$new('FirstIteratorState'), items_path='$', max_concurrency=0)
  expect_equal(
    map_state$to_list(),
    list(
      'Iterator'=list(
        'StartAt'='FirstIteratorState',
        'States'=list(
          'FirstIteratorState'=list(
            'Type'='Pass',
            'End'=TRUE)
        )
      ),
      'ItemsPath'='$',
      'MaxConcurrency'=0,
      'Type'='Map',
      'End'=TRUE
    )
  )
})

test_that("test nested chain is now allowed", {
  chain = Chain$new(c(Chain$new(c(Pass$new('S1')))))

  expect_equal(length(chain$steps), 1)
})

test_that("test catch creation", {
  catch = Catch$new(error_equals=list('States.ALL'), next_step=Fail$new('End'))
  expect_equal(
    catch$to_list(),
    list(
      'ErrorEquals'=list('States.ALL'),
      'Next'='End'
    )
  )
})

test_that("test append states after terminal state will fail", {

  expect_error({
    chain = Chain$new()
    chain$append(Pass$new('Pass'))
    chain$append(Fail$new('Fail'))
    chain$append(Pass$new('Pass2'))
  },".*`Fail` does not support method `.next`")

  expect_error({
    chain = Chain$new()
    chain$append(Pass$new('Pass'))
    chain$append(Succeed$new('Succeed'))
    chain$append(Pass$new('Pass2'))
  }, ".*`Succeed` does not support method `.next`")
})

test_that("test chaining steps", {
  s1 = Pass$new('Step - One')
  s2 = Pass$new('Step - Two')
  s3 = Pass$new('Step - Three')

  Chain$new(c(s1, s2))
  expect_equal(s1$next_step, s2)
  expect_null(s2$next_step)

  chain1 = Chain$new(c(s2, s3))
  expect_equal(s2$next_step, s3)

  chain2 = Chain$new(c(s1, s3))
  expect_equal(s1$next_step, s3)
  expect_equal(s2$next_step, s1$next_step)

  expect_error(
    chain2$append(s3),
    ".*A chain cannot have duplicate states.")

  expect_error(
    chain3 = Chain$new(c(chain1, chain2)))

  s1$.next(s2)

  chain3 = Chain$new(c(s3, s1))
  expect_equal(chain3$steps, c(s3, s1))
  expect_equal(s3$next_step, s1)
  expect_equal(s1$next_step, s2)
  expect_equal(s2$next_step, s3)

  Chain$new(c(Chain$new(c(s3)), Chain$new(c(s1))))

  expect_error({
    Chain$new(c(Chain$new(c(s1, s2, s1)), s3))
    Chain$new(c(s1, s2, s1, s3))
  })

  Chain$new(c(Chain$new(c(s1, s2)), s3))
  expect_equal(s1$next_step, s2)
  expect_equal(s2$next_step, s3)
})

test_that("test chaining choice sets default field", {
  s1_pass = Pass$new('Step - One')
  s2_choice = Choice$new('Step - Two')
  s3_pass = Pass$new('Step - Three')

  chain1 = Chain$new(c(s1_pass, s2_choice, s3_pass))
  expect_equal(chain1$steps, c(s1_pass, s2_choice, s3_pass))
  expect_equal(s1_pass$next_step, s2_choice)
  expect_equal(s2_choice$default, s3_pass)
  expect_null(s2_choice$next_step)  # Choice steps do not have next_step
  expect_null(s3_pass$next_step)
})

test_that("test chaining choice with existing default overrides value", {
  s1_pass = Pass$new('Step - One')
  s2_choice = Choice$new('Step - Two')
  s3_pass = Pass$new('Step - Three')

  s2_choice$default_choice(s3_pass)

  Chain$new(c(s2_choice, s1_pass))
  actual_warning = LOGGER$last_event
  expected_warning = sprintf("Chaining Choice state: Overwriting (%s)'s current default_choice (%s) with (%s)",
                             s2_choice$state_id, s3_pass$state_id, s1_pass$state_id)
  expect_equal(actual_warning$msg, expected_warning)
  expect_equal(unname(actual_warning$level_name),"warn")
  expect_equal(s2_choice$default, s1_pass)
  expect_null(s2_choice$next_step) # Choice steps do not have next_step
})

test_that("test catch fail for unsupported state", {
  s1 = Pass$new('Step - One')
  expect_error(
    s1$add_retry(Retry$new(
      error_equals=c('ErrorA', 'ErrorB'),
      interval_seconds=1,
      max_attempts=2,
      backoff_rate=2))
  )
})

test_that("test retry fail for unsupported state", {
  c1 = Choice$new('My Choice')
  expect_error(
    c1$add_catch(Catch$new(
      error_equals="States.NoChoiceMatched",
      next_step=Fail$new("ChoiceFailed")))
  )
})
