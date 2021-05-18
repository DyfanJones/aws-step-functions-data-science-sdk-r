# NOTE: This code has been modified from AWS Stepfunctions Python:
# https://github.com/aws/aws-step-functions-data-science-sdk-python/blob/main/src/stepfunctions/steps/sagemaker.py

#' @include inputs_placeholders.R
#' @include steps_states.R
#' @include steps_utils.R
#' @include utils.R

#' @import R6

#' @title Sagemaker TrainingStep task class
#' @description Creates a Task State to execute a `SageMaker Training Job`
#'              \url{https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateTrainingJob.html}
#'              The TrainingStep will also create a model by default, and the model shares
#'              the same name as the training job.
#' @export
TrainingStep = R6Class("TrainingStep",
  inherit = Task,
  public = list(

    #' @description Initialize TrainingStep class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'              to 128 unicode characters. State names **must be** unique
    #'              within the scope of the whole state machine.
    #' @param estimator (sagemaker.estimator.EstimatorBase): The estimator for the training
    #'              step. Can be a `BYO estimator, Framework estimator`
    #'              \url{https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html}
    #'              or `Amazon built-in algorithm estimator`
    #'              \url{https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html}.
    #' @param job_name (str or Placeholder): Specify a training job name, this is required for
    #'              the training job to run. We recommend to use :py:class:`~stepfunctions.inputs.ExecutionInput`
    #'              placeholder collection to pass the value dynamically in each execution.
    #' @param data : Information about the training data. Please refer to the ``fit()``
    #'              method of the associated estimator, as this can take any of the following forms:
    #'              \itemize{
    #'                 \item{(str) - The S3 location where training data is saved.}
    #'                 \item{(list[str, str] or list[str, sagemaker.inputs.TrainingInput]) - If using multiple
    #'                       channels for training data, you can specify a list mapping channel names to
    #'                       strings or :func:`~sagemaker.inputs.TrainingInput` objects.}
    #'                 \item{(sagemaker.inputs.TrainingInput) - Channel configuration for S3 data sources that can
    #'                       provide additional information about the training dataset. See
    #'                       :func:`sagemaker.inputs.TrainingInput` for full details.}
    #'                 \item{(sagemaker.amazon.amazon_estimator.RecordSet) - A collection of
    #'                       Amazon :class:`Record` objects serialized and stored in S3.
    #'                       For use with an estimator for an Amazon algorithm.}
    #'                 \item{(list[sagemaker.amazon.amazon_estimator.RecordSet]) - A list of
    #'                       :class:`sagemaker.amazon.amazon_estimator.RecordSet` objects,
    #'                       where each instance is a different channel of training data.}
    #'              }
    #' @param hyperparameters (list, optional): Specify the hyper parameters for the training. (Default: None)
    #' @param mini_batch_size (int): Specify this argument only when estimator is a built-in
    #'              estimator of an Amazon algorithm. For other estimators, batch size should be
    #'              specified in the estimator.
    #' @param experiment_config (list, optional): Specify the experiment config for the training. (Default: None)
    #' @param wait_for_completion (bool, optional): Boolean value set to `True` if the Task state
    #'              should wait for the training job to complete before proceeding to the next
    #'              step in the workflow. Set to `False` if the Task state should submit the
    #'              training job and proceed to the next step. (default: True)
    #' @param tags (list[list], optional): List to tags \url{https://docs.aws.amazon.com/sagemaker/latest/dg/API_Tag.html}
    #'              to associate with the resource.
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          estimator,
                          job_name,
                          data=NULL,
                          hyperparameters=NULL,
                          mini_batch_size=NULL,
                          experiment_config=NULL,
                          wait_for_completion=TRUE,
                          tags=NULL,
                          ...){
      self$estimator = estimator
      self$job_name = job_name
      kwargs = list(...)

      # get methods from R6sagemaker
      AirFlowWorkFlow = pkg_method("AirFlowWorkFlow", "R6sagemaker")

      if (wait_for_completion)
        kwargs[[Field$Resource]] = 'arn:aws:states:::sagemaker:createTrainingJob.sync'
      else
        kwargs[[Field$Resource]]= 'arn:aws:states:::sagemaker:createTrainingJob'

      if (is.character(job_name))
        parameters = AirFlowWorkFlow$new()$training_config(
          estimator=estimator, inputs=data, job_name=job_name, mini_batch_size=mini_batch_size)
      else
        parameters = AirFlowWorkFlow$new()$training_config(
          estimator=estimator, inputs=data, mini_batch_size=mini_batch_size)

      if (!is.null(estimator$debugger_hook_config) && !isFALSE(estimator$debugger_hook_config))
        parameters[['DebugHookConfig']] = estimator$debugger_hook_config$to_request_list()

      if (!is.null(estimator$rules))
        parameters[['DebugRuleConfigurations']] = lapply(estimator$rules, function(rule) rule$to_debugger_rule_config_dict())

      if (inherits(job_name, c("ExecutionInput", "StepInput")))
        parameters[['TrainingJobName']] = job_name

      parameters[['HyperParameters']] = hyperparameters
      parameters[['ExperimentConfig']] = experiment_config

      if ('S3Operations' %in% names(parameters))
        parameters[['S3Operations']] = NULL

      if (!is.null(tags))
        parameters[['Tags']] = tags_dict_to_kv_list(tags)

      kwargs[[Field$Parameters]] = parameters
      kwargs[["state_id"]]=state_id
      do.call(super$initialize, kwargs)
    },

    #' @description Build Sagemaker model representation of the expected trained
    #'              model from the Training step. This can be passed
    #'              to the ModelStep to save the trained model in Sagemaker.
    #' @param model_name (str, optional): Specify a model name. If not provided, training job name will be used as the model name.
    #' @return sagemaker.model.Model: Sagemaker model representation of the expected trained model.
    get_expected_model = function(model_name=NULL){
      model = self$estimator$create_model()
      if (!is.null(model_name))
        model.name = model_name
      else
        model$name = self$job_name
      model$model_data = self$output()[["ModelArtifacts"]][["S3ModelArtifacts"]]
      return(model)
    }
  ),
  lock_objects=F
)

#' @title Sagemaker TransfromStep task class
#' @description Creates a Task State to execute a `SageMaker Transform Job`
#'              \url{https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateTransformJob.html}
#' @export
TransformStep = R6Class("TransformStep",
  inherit = Task,
  public = list(

    #' @description Initialize TranzformStep class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'              to 128 unicode characters. State names **must be** unique within the
    #'              scope of the whole state machine.
    #' @param transformer (sagemaker.transformer.Transformer): The SageMaker transformer
    #'              to use in the TransformStep.
    #' @param job_name (str or Placeholder): Specify a transform job name. We recommend to use
    #'             :py:class:`~stepfunctions.inputs.ExecutionInput` placeholder collection
    #'             to pass the value dynamically in each execution.
    #' @param model_name (str or Placeholder): Specify a model name for the transform job
    #'             to use. We recommend to use :py:class:`~stepfunctions.inputs.ExecutionInput`
    #'             placeholder collection to pass the value dynamically in each execution.
    #' @param data (str): Input data location in S3.
    #' @param data_type (str): What the S3 location defines (default: 'S3Prefix').
    #'               Valid values:
    #'               \itemize{
    #'                   \item{'S3Prefix' - the S3 URI defines a key name prefix. All objects with this prefix will
    #'                         be used as inputs for the transform job}
    #'                   \item{'ManifestFile' - the S3 URI points to a single manifest file listing each S3 object
    #'                         to use as an input for the transform job.}
    #'               }
    #' @param content_type (str): MIME type of the input data (default: None).
    #' @param compression_type (str): Compression type of the input data, if compressed
    #'               (default: None). Valid values: 'Gzip', None.
    #' @param split_type (str): The record delimiter for the input object (default: 'None').
    #'                Valid values: 'None', 'Line', 'RecordIO', and 'TFRecord'.
    #' @param experiment_config (list, optional): Specify the experiment config for the transform. (Default: None)
    #' @param wait_for_completion (bool, optional): Boolean value set to `True` if the Task state
    #'                should wait for the transform job to complete before proceeding to the next
    #'                step in the workflow. Set to `False` if the Task state should submit the transform
    #'                job and proceed to the next step. (default: True)
    #' @param tags (list[list], optional): List to tags \url{https://docs.aws.amazon.com/sagemaker/latest/dg/API_Tag.html}
    #'                to associate with the resource.
    #' @param input_filter (str): A JSONPath to select a portion of the input to pass to
    #'                the algorithm container for inference. If you omit the field, it gets the
    #'                value ‘$’, representing the entire input. For CSV data, each row is
    #'                taken as a JSON array, so only index-based JSONPaths can be applied,
    #'                e.g. $[0], $[1:]. CSV data should follow the RFC format. See Supported
    #'                JSONPath Operators for a table of supported JSONPath operators. For more
    #'                information, see the SageMaker API documentation for CreateTransformJob. Some examples:
    #'                “$[1:]”, “$.features” (default: None).
    #' @param output_filter (str): A JSONPath to select a portion of the joined/original
    #'                output to return as the output. For more information, see the SageMaker API
    #'                documentation for CreateTransformJob. Some examples: “$[1:]”,
    #'                “$.prediction” (default: None).
    #' @param join_source (str): The source of data to be joined to the transform output.
    #'                It can be set to ‘Input’ meaning the entire input record will be joined to
    #'                the inference result. You can use OutputFilter to select the useful portion
    #'                before uploading to S3. (default: None). Valid values: Input, None.
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          transformer,
                          job_name,
                          model_name,
                          data,
                          data_type='S3Prefix',
                          content_type=NULL,
                          compression_type=NULL,
                          split_type=NULL,
                          experiment_config=NULL,
                          wait_for_completion=TRUE,
                          tags=NULL,
                          input_filter=NULL,
                          output_filter=NULL,
                          join_source=NULL,
                          ...){
      kwargs = list(...)
      if (wait_for_completion)
        kwargs[[Field$Resource]] = 'arn:aws:states:::sagemaker:createTransformJob.sync'
      else
        kwargs[[Field$Resource]] = 'arn:aws:states:::sagemaker:createTransformJob'

      # get methods from R6sagemaker
      AirFlowWorkFlow = pkg_method("AirFlowWorkFlow", "R6sagemaker")
      if (is.character(job_name)){
        parameters = AirFlowWorkFlow$new()$transform_config(
          transformer=transformer,
          data=data,
          data_type=data_type,
          content_type=content_type,
          compression_type=compression_type,
          split_type=split_type,
          job_name=job_name,
          input_filter=input_filter,
          output_filter=output_filter,
          join_source=join_source
        )
      } else {
        parameters = AirFlowWorkFlow$new()$transform_config(
          transformer=transformer,
          data=data,
          data_type=data_type,
          content_type=content_type,
          compression_type=compression_type,
          split_type=split_type,
          input_filter=input_filter,
          output_filter=output_filter,
          join_source=join_source
        )
      }

      if (inherits(job_name, c("ExecutionInput", "StepInput")))
        parameters[['TransformJobName']] = job_name

      parameters[['ModelName']] = model_name
      parameters['ExperimentConfig'] = experiment_config

      if (!is.null(tags))
        parameters['Tags'] = tags_dict_to_kv_list(tags)

      kwargs[[Field$Parameters]] = parameters
      kwargs[["state_id"]] = state_id
      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title Sagemaker ModelStep task class
#' @description Creates a Task State to `create a model in SageMaker`
#'              \url{https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateModel.html}.
#' @export
ModelStep = R6Class("ModelStep",
  inherit = Task,
  public = list(

    #' @description Initialize ModelStep class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'              to 128 unicode characters. State names **must be** unique within
    #'              the scope of the whole state machine.
    #' @param model (sagemaker.model.Model): The SageMaker model to use in the ModelStep.
    #'              If :py:class:`TrainingStep` was used to train the model and saving the
    #'              model is the next step in the workflow, the output of :py:func:
    #'              `TrainingStep.get_expected_model()` can be passed here.
    #' @param model_name (str or Placeholder, optional): Specify a model name, this
    #'              is required for creating the model. We recommend to use :py:class:
    #'              `~stepfunctions.inputs.ExecutionInput` placeholder collection to pass
    #'              the value dynamically in each execution.
    #' @param instance_type (str, optional): The EC2 instance type to deploy this Model
    #'              to. For example, 'ml.p2.xlarge'.
    #' @param tags (list[list], optional): `List to tags`
    #'              \url{https://docs.aws.amazon.com/sagemaker/latest/dg/API_Tag.html}
    #'              to associate with the resource.
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          model,
                          model_name=NULL,
                          instance_type=NULL,
                          tags=NULL,
                          ...){
      kwargs = list(...)

      # get methods from R6sagemaker
      AirFlowWorkFlow = pkg_method("AirFlowWorkFlow", "R6sagemaker")

      if (inherits(model, "FrameworkModel")){
        parameters = AirFlowWorkFlow$new()$model_config(
          model=model, instance_type=instance_type, role=model$role, image_uri=model$image_uri)
        if (!is.null(model_name))
          parameters[['ModelName']] = model_name
      } else if (inherits(model, "Model")){
        parameters = list(
          'ExecutionRoleArn'=model$role,
          'ModelName'=model_name %||% model$name,
          'PrimaryContainer'=list(
            'Environment'=list(),
            'Image'=model$image_uri,
            'ModelDataUrl'=model$model_data
          )
        )
      } else {
        stop(sprintf(
          "Expected 'model' parameter to be of type 'sagemaker.model.Model', but received type '%s'",
          class(model)[1]))
      }

      if ('S3Operations' %in% names(parameters))
        parameters[['S3Operations']] = NULL

      if (!is.null(tags))
        parameters[['Tags']] = tags_dict_to_kv_list(tags)

      kwargs[[Field$Parameters]] = parameters
      kwargs[[Field$Resource]] = 'arn:aws:states:::sagemaker:createModel'
      kwargs[["state_id"]] = state_id

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title Sagemaker EndpointConfigStep task class
#' @description Creates a Task State to `create an endpoint configuration in SageMaker`
#'              \url{https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpointConfig.html}.
#' @export
EndpointConfigStep = R6Class("EndpointConfigStep",
  inherit = Task,
  public = list(

    #' @description Initialize EndpointConfigStep class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'              to 128 unicode characters. State names **must be** unique within the
    #'              scope of the whole state machine.
    #' @param endpoint_config_name (str or Placeholder): The name of the endpoint
    #'              configuration to create. We recommend to use :py:class:
    #'              `~stepfunctions.inputs.ExecutionInput` placeholder collection to pass
    #'              the value dynamically in each execution.
    #' @param model_name (str or Placeholder): The name of the SageMaker model to attach
    #'              to the endpoint configuration. We recommend to use :py:class:
    #'              `~stepfunctions.inputs.ExecutionInput` placeholder collection to pass
    #'              the value dynamically in each execution.
    #' @param initial_instance_count (int or Placeholder): The initial number of instances
    #'              to run in the ``Endpoint`` created from this ``Model``.
    #' @param instance_type (str or Placeholder): The EC2 instance type to deploy this
    #'              Model to. For example, 'ml.p2.xlarge'.
    #' @param variant_name (str, optional): The name of the production variant.
    #' @param data_capture_config (sagemaker.model_monitor.DataCaptureConfig, optional): Specifies
    #'              configuration related to Endpoint data capture for use with
    #'              Amazon SageMaker Model Monitoring. Default: None.
    #' @param tags (list[list], optional): `List to tags`
    #'              \url{https://docs.aws.amazon.com/sagemaker/latest/dg/API_Tag.html}
    #'              to associate with the resource.
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          endpoint_config_name,
                          model_name,
                          initial_instance_count,
                          instance_type,
                          variant_name='AllTraffic',
                          data_capture_config=NULL,
                          tags=NULL,
                          ...){
      kwargs = list(...)

      parameters = list(
        'EndpointConfigName'=endpoint_config_name,
        'ProductionVariants'=list(list(
          'InitialInstanceCount'=initial_instance_count,
          'InstanceType'=instance_type,
          'ModelName'=model_name,
          'VariantName'=variant_name)
        )
      )

      if (inherits(data_capture_config, "DataCaptureConfig"))
        parameters[['DataCaptureConfig']] = data_capture_config$to_request_dict()

      if (!is.null(tags))
        parameters[['Tags']] = tags_dict_to_kv_list(tags)

      kwargs[[Field$Resource]] = 'arn:aws:states:::sagemaker:createEndpointConfig'
      kwargs[[Field$Parameters]] = parameters
      kwargs[["state_id"]] = state_id

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title Sagemaker EndpointStep task class
#' @description Creates a Task State to create \url{https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateEndpoint.html}
#'              or update \url{https://docs.aws.amazon.com/sagemaker/latest/dg/API_UpdateEndpoint.html}
#'              an endpoint in SageMaker.
#' @export
EndpointStep = R6Class("EndpointStep",
  inherit = Task,
  public = list(

    #' @description Initialize EndpointStep Class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'              to 128 unicode characters. State names **must be** unique within
    #'              the scope of the whole state machine.
    #' @param endpoint_name (str or Placeholder): The name of the endpoint to create.
    #'              We recommend to use :py:class:`~stepfunctions.inputs.ExecutionInput`
    #'              placeholder collection to pass the value dynamically in each execution.
    #' @param endpoint_config_name (str or Placeholder): The name of the endpoint configuration
    #'              to use for the endpoint. We recommend to use :py:class:
    #'              `~stepfunctions.inputs.ExecutionInput` placeholder collection to pass the
    #'              value dynamically in each execution.
    #' @param tags (list[list], optional): `List to tags`
    #'              \url{https://docs.aws.amazon.com/sagemaker/latest/dg/API_Tag.html}
    #'              to associate with the resource.
    #' @param update (bool, optional): Boolean flag set to `True` if endpoint must to
    #'              be updated. Set to `False` if new endpoint must be created. (default: False)
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          endpoint_name,
                          endpoint_config_name,
                          tags=NULL,
                          update=FALSE,
                          ...){
      kwargs = list(...)

      parameters = list(
        "EndpointConfigName"=endpoint_config_name,
        "EndpointName"=endpoint_name)

      if (!is.null(tags))
        parameters[['Tags']] = tags_dict_to_kv_list(tags)

      if (update){
        kwargs[[Field$Resource]] = 'arn:aws:states:::sagemaker:updateEndpoint'
      }else{
        kwargs[[Field$Resource]] = 'arn:aws:states:::sagemaker:createEndpoint'
      }

      kwargs[[Field$Parameters]] = parameters
      kwargs[["state_id"]] = state_id

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title Sagemaker TuningStep task class
#' @description Creates a Task State to execute a SageMaker HyperParameterTuning Job.
#' @export
TuningStep = R6Class("TuningStep",
  inherit = Task,
  public = list(

    #' @description Initialize TuningStep class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'              to 128 unicode characters. State names **must be** unique within
    #'              the scope of the whole state machine.
    #' @param tuner (sagemaker.tuner.HyperparameterTuner): The tuner to use in the TuningStep.
    #' @param job_name (str or Placeholder): Specify a tuning job name.  We recommend
    #'              to use :py:class:`~stepfunctions.inputs.ExecutionInput` placeholder
    #'              collection to pass the value dynamically in each execution.
    #' @param data : Information about the training data. Please refer to the ``fit()``
    #'              method of the associated estimator in the tuner, as this can take
    #'              any of the following forms:
    #'              \itemize{
    #'                  \item{(str) - The S3 location where training data is saved.}
    #'                  \item{(list[str, str] or list[str, sagemaker.inputs.TrainingInput]) - If using multiple
    #'                        channels for training data, you can specify a list mapping channel names to
    #'                        strings or :func:`~sagemaker.inputs.TrainingInput` objects.}
    #'                  \item{(sagemaker.inputs.TrainingInput) - Channel configuration for S3 data sources that can
    #'                        provide additional information about the training dataset. See
    #'                        :func:`sagemaker.inputs.TrainingInput` for full details.}
    #'                  \item{(sagemaker.amazon.amazon_estimator.RecordSet) - A collection of
    #'                        Amazon :class:`Record` objects serialized and stored in S3.
    #'                        For use with an estimator for an Amazon algorithm.}
    #'                  \item{(list[sagemaker.amazon.amazon_estimator.RecordSet]) - A list of
    #'                        :class:`sagemaker.amazon.amazon_estimator.RecordSet` objects,
    #'                        where each instance is a different channel of training data.}
    #'              }
    #' @param wait_for_completion (bool, optional): Boolean value set to `True` if
    #'               the Task state should wait for the tuning job to complete before
    #'               proceeding to the next step in the workflow. Set to `False` if the
    #'               Task state should submit the tuning job and proceed to the next step. (default: True)
    #' @param tags (list[list], optional): List to tags
    #'               \url{https://docs.aws.amazon.com/sagemaker/latest/dg/API_Tag.html} to
    #'               associate with the resource.
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          tuner,
                          job_name,
                          data,
                          wait_for_completion=TRUE,
                          tags=NULL,
                          ...){
      kwargs = list(...)

      # get methods from R6sagemaker
      AirFlowWorkFlow = pkg_method("AirFlowWorkFlow", "R6sagemaker")

      if (wait_for_completion)
        kwargs[[Field$Resource]] = 'arn:aws:states:::sagemaker:createHyperParameterTuningJob.sync'
      else
        kwargs[[Field$Resource]] = 'arn:aws:states:::sagemaker:createHyperParameterTuningJob'

      parameters = AirFlowWorkFlow$new()$tuning_config(tuner=tuner, inputs=data, job_name=job_name)

      if (!is.null(job_name))
        parameters[['HyperParameterTuningJobName']] = job_name

      if ('S3Operations' %in% names(parameters))
        parameters[['S3Operations']] = NULL

      if (!is.null(tags))
        parameters[['Tags']] = tags_dict_to_kv_list(tags)

      kwargs[[Field$Parameters]] = parameters
      kwargs[["state_id"]] = state_id

      do.call(super$initialize, kwargs)
    }
  ),
  lock_objects=F
)

#' @title Sagemaker ProcessingStep task class
#' @description Creates a Task State to execute a SageMaker Processing Job.
#' @export
ProcessingStep = R6Class("ProcessingStep",
  inherit = Task,
  public = list(

    #' @description Initialize ProcessingStep class
    #' @param state_id (str): State name whose length **must be** less than or equal
    #'              to 128 unicode characters. State names **must be** unique within
    #'              the scope of the whole state machine.
    #' @param processor (sagemaker.processing.Processor): The processor for the processing step.
    #' @param job_name (str or Placeholder): Specify a processing job name, this is
    #'              required for the processing job to run. We recommend to use
    #'              :py:class:`~stepfunctions.inputs.ExecutionInput` placeholder
    #'              collection to pass the value dynamically in each execution.
    #' @param inputs (list[:class:`~sagemaker.processing.ProcessingInput`]): Input files for
    #'              the processing job. These must be provided as
    #'              :class:`~sagemaker.processing.ProcessingInput` objects (default: None).
    #' @param outputs (list[:class:`~sagemaker.processing.ProcessingOutput`]): Outputs for
    #'              the processing job. These can be specified as either path strings or
    #'              :class:`~sagemaker.processing.ProcessingOutput` objects (default: None).
    #' @param experiment_config (list, optional): Specify the experiment config for
    #'              the processing. (Default: None)
    #' @param container_arguments ([str]): The arguments for a container used to run a processing job.
    #' @param container_entrypoint ([str]): The entrypoint for a container used to run a processing job.
    #' @param kms_key_id (str): The AWS Key Management Service (AWS KMS) key that Amazon SageMaker
    #'              uses to encrypt the processing job output. KmsKeyId can be an ID of a KMS key,
    #'              ARN of a KMS key, alias of a KMS key, or alias of a KMS key.
    #'              The KmsKeyId is applied to all outputs.
    #' @param wait_for_completion (bool, optional): Boolean value set to `True` if
    #'              the Task state should wait for the processing job to complete before
    #'              proceeding to the next step in the workflow. Set to `False` if the Task
    #'              state should submit the processing job and proceed to the next step. (default: True)
    #' @param tags (list[list], optional): `List to tags`
    #'             \url{https://docs.aws.amazon.com/sagemaker/latest/dg/API_Tag.html}
    #'             to associate with the resource.
    #' @param ... : Extra Fields passed to Task class
    initialize = function(state_id,
                          processor,
                          job_name,
                          inputs=NULL,
                          outputs=NULL,
                          experiment_config=NULL,
                          container_arguments=NULL,
                          container_entrypoint=NULL,
                          kms_key_id=NULL,
                          wait_for_completion=TRUE,
                          tags=NULL,
                          ...){
      kwargs=list(...)

      # get methods from R6sagemaker
      AirFlowWorkFlow = pkg_method("AirFlowWorkFlow", "R6sagemaker")

      if (wait_for_completion)
        kwargs[[Field$Resource]] = 'arn:aws:states:::sagemaker:createProcessingJob.sync'
      else
        kwargs[[Field$Resource]] = 'arn:aws:states:::sagemaker:createProcessingJob'

      if (is.character(job_name)){
        parameters = AirFlowWorkFlow$new()$processing_config(
          processor=processor,
          inputs=inputs,
          outputs=outputs,
          container_arguments=container_arguments,
          container_entrypoint=container_entrypoint,
          kms_key_id=kms_key_id,
          job_name=job_name)
      } else{
        parameters = AirFlowWorkFlow$new()$processing_config(
          processor=processor,
          inputs=inputs,
          outputs=outputs,
          container_arguments=container_arguments,
          container_entrypoint=container_entrypoint,
          kms_key_id=kms_key_id)
      }
      if (inherits(job_name, c("ExecutionInput", "StepInput")))
        parameters[['ProcessingJobName']] = job_name

      if (!is.null(experiment_config))
        parameters[['ExperimentConfig']] = experiment_config

      if (!is.null(tags))
        parameters[['Tags']] = tags_dict_to_kv_list(tags)

      if ('S3Operations' %in% names(parameters))
        parameters[['S3Operations']] = NULL

      kwargs[[Field$Parameters]] = parameters
      kwargs[["state_id"]] = state_id

      do.call(super$initialize, kwargs)
    }
  )
)
