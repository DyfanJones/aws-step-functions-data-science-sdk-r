# aws-step-functions-data-science-sdk-r
Step Functions Data Science SDK for building machine learning (ML) workflows and pipelines on AWS. This package utilises [`paws`](https://github.com/paws-r/paws) to make a connection to `AWS`.

# Install:

Cran Version
```
# TBC
```

Dev Version
```
remotes::install_github("DyfanJones/aws-step-functions-data-science-sdk-r")
```

## Building a Workflow
**Note this example is taken from: https://github.com/aws/aws-step-functions-data-science-sdk-python**

### Steps

You create steps using the SDK, and chain them together into sequential workflows. Then, you can create those workflows in AWS Step Functions and execute them in Step Functions directly from your R code. For example, the following is how you define a pass step.

```
library(stepfunctions)
```

```
start_pass_state = Pass$new(
    state_id="MyPassState"
)
```

The following is how you define a wait step.

```
wait_state = Wait$new(
    state_id="Wait for 3 seconds",
    seconds=3
)
```

The following example shows how to define a Lambda step, and then defines a Retry and a Catch.
```
lambda_state = LambdaStep$new(
  state_id="Convert HelloWorld to Base64",
  parameters=list(
    "FunctionName"="MyLambda", #replace with the name of your function
    "Payload"=list(
      "input"="HelloWorld")
  )
)
```

```
lambda_state$add_retry(Retry$new(
  error_equals="States.TaskFailed",
  interval_seconds=15,
  max_attempts=2,
  backoff_rate=4.0
))
```
```
lambda_state$add_catch(Catch$new(
  error_equals="States.TaskFailed",
  next_step=Fail$new("LambdaTaskFailed")
))
```

### Workflows

After you define these steps, chain them together into a logical sequence.

```
workflow_definition=Chain$new(c(start_pass_state, wait_state, lambda_state))

```

Once the steps are chained together, you can define the workflow definition.

```
# change execution role to your execution role
stepfunctions_execution_role="dummy-role"
workflow = Workflow$new(
  name="MyWorkflow_v1234",
  definition=workflow_definition,
  role=stepfunctions_execution_role
)
```

### Visualizing a Workflow

The following generates a graphical representation of your workflow. Please note that visualization currently only works in Jupyter notebooks. Visualization is not available in JupyterLab.

```
workflow$render_graph()
```

**NOTE:** Currently graphs can only render in jupyter notebook. Rendering graphs in Rstudio is on the todo list :)

## Review a Workflow Definition

The following renders the JSON of the Amazon States Language definition of the workflow you created.

```
workflow$definition$to_json(pretty=TRUE)
```

```
{
  "StartAt": "MyPassState",
  "States": {
    "MyPassState": {
      "Type": "Pass",
      "Next": "Wait for 3 seconds"
    },
    "Wait for 3 seconds": {
      "Seconds": 3,
      "Type": "Wait",
      "Next": "Convert HelloWorld to Base64"
    },
    "Convert HelloWorld to Base64": {
      "Parameters": {
        "FunctionName": "MyLambda",
        "Payload": {
          "input": "HelloWorld"
        }
      },
      "Resource": "arn:aws:states:::lambda:invoke",
      "Type": "Task",
      "End": true,
      "Retry": [
        {
          "Error_equals": [
            "States.TaskFailed"
          ],
          "Interval_seconds": 15,
          "Max_attempts": 2,
          "Backoff_rate": 4
        }
      ],
      "Catch": [
        {
          "Error_equals": "States.TaskFailed",
          "Next": "LambdaTaskFailed"
        }
      ]
    },
    "LambdaTaskFailed": {
      "Type": "Fail"
    }
  }
}
```
