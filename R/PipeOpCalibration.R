#' @title Calibration Pipeline Operator
#'
#' @usage NULL
#' @name mlr_pipeops_calibration
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOp`][mlr3pipelines::PipeOp].
#'
#' @description
#' Pipeline operator for calibrating classification models using different calibration methods.
#' Supports Platt scaling, isotonic regression, and beta calibration.
#' This operator adjusts the predicted probabilities to better reflect the true probabilities of the outcomes.
#'
#' @section Construction:
#' ```
#' PipeOpCalibration$new(id = "calibration", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of the resulting object, default `"calibration"`.
#' * `learner` :: [`Learner`][mlr3::Learner]\cr
#'   Base learner to be calibrated.
#' * `method` :: `character(1)`\cr
#'   Calibration method to use. One of `"platt"`, `"isotonic"`, or `"beta"`. Default is `"platt"`.
#' * `rsmp` :: [`Resampling`][mlr3::Resampling]\cr
#'   Resampling strategy for cross-validation. Default is `rsmp("cv", folds = 5)`.
#' * `rr` :: [`ResampleResult`][mlr3::ResampleResult]\cr
#'   Resample result object, if provided.
#' * `parameters` :: `character(1)`\cr
#'   Parameters for beta calibration. Default is `"abm"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the default hyperparameter settings.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOp`][mlr3pipelines::PipeOp].
#'
#' * Input during training: [`Task`][mlr3::Task] with the data to calibrate.
#' * Output during training: `NULL`.
#' * Input during prediction: [`Task`][mlr3::Task] with new data.
#' * Output during prediction: [`PredictionClassif`][mlr3::PredictionClassif] with calibrated probabilities.
#'
#' @section State:
#' The `$state` is a named `list` with the elements inherited from [`PipeOp`][mlr3pipelines::PipeOp].
#'
#' @section Parameters:
#' The parameters are inherited from [`PipeOp`][mlr3pipelines::PipeOp], as well as:
#'
#' * `learner` :: [`Learner`][mlr3::Learner]\cr
#'   Base learner to be calibrated.
#' * `method` :: `character(1)`\cr
#'   Calibration method to use.
#' * `rsmp` :: [`Resampling`][mlr3::Resampling]\cr
#'   Resampling strategy for cross-validation.
#' * `rr` :: [`ResampleResult`][mlr3::ResampleResult]\cr
#'   Resample result object, if provided.
#' * `parameters` :: `character(1)`\cr
#'   Parameters for beta calibration.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings.
#'
#' @section Fields:
#' * `learner` :: [`Learner`][mlr3::Learner]\cr
#'   Base learner to be calibrated.
#' * `method` :: `character(1)`\cr
#'   Calibration method used.
#' * `rsmp` :: [`Resampling`][mlr3::Resampling]\cr
#'   Resampling strategy.
#' * `learners` :: `list`\cr
#'   List of learners obtained from resampling.
#' * `calibrators` :: `list`\cr
#'   List of calibrator models.
#' * `rr` :: [`ResampleResult`][mlr3::ResampleResult]\cr
#'   Resample result object.
#' * `parameters` :: `character(1)`\cr
#'   Parameters for beta calibration.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`][mlr3pipelines::PipeOp].
#'
#' @family PipeOps
#' @template seealso_pipeops_list
#' @include PipeOp.R
#' @export
#'
#' @examples
#' # Example usage of PipeOpCalibration
#' # Load a binary classification task
#' set.seed(1)
#' data("Sonar", package = "mlbench")
#' task = as_task_classif(Sonar, target = "Class", positive = "M")
#' splits = partition(task)
#' task_train = task$clone()$filter(splits$train)
#' task_test = task$clone()$filter(splits$test)
#'
#' # Initialize the uncalibrated learner
#' learner_uncal <- lrn("classif.ranger", predict_type = "prob")
#' # Initialize the calibrated learner
#' rsmp <- rsmp("cv", folds = 5)
#' learner_cal <- as_learner(po("calibration_cv", learner = learner_uncal,
#'                              method = "platt", rsmp = rsmp))
#' # Set ID's for the learners
#' learner_uncal$id <- "Uncalibrated Learner"
#' learner_cal$id <- "Calibrated Learner
#'
#' # Train the learners
#' learner_uncal$train(task_train)
#' learner_cal$train(task_train)

PipeOpCalibration <- R6::R6Class(
  "PipeOpCalibration",
  inherit = mlr3pipelines::PipeOp,

  public = list(
    learner = NULL,
    method = NULL,
    rsmp = NULL,
    learners = NULL,
    calibrators = NULL,
    rr = NULL,
    parameters = NULL,

    initialize = function(
    learner = NULL,
    method = "platt",
    rsmp = NULL,
    rr = NULL,
    parameters = "abm",
    param_vals = list()) {

      if (is.null(learner) && is.null(rr)) {
        stop("Either learner or rr object must be provided.")
      }
      if (!is.null(rr)) {
        self$rr = rr
      }
      if (!is.null(learner)) {
        self$learner = learner$clone()
        id = self$learner$base_learner()$id
      } else {
        self$learner = self$rr$learners[[1]]$clone()
      }
      if (!is.null(rsmp)) {
        self$rsmp = rsmp
      } else if (is.null(rsmp) && is.null(rr)) {
        self$rsmp = rsmp("cv", folds = 5)
      }
      if (self$learner$predict_type != "prob") {
        stop("predict_type has to be 'prob'")
      }
      self$method = method
      self$parameters = parameters
      self$learners = list()
      self$calibrators = list()
      super$initialize(id = self$learner$base_learner()$id,
                       param_set = alist(self$learner$base_learner()$param_set),
                       param_vals = param_vals,
                       input = data.table(name = "input", train = "Task",
                                          predict = "Task"),
                       output = data.table(name = "output", train = "NULL",
                                           predict = "PredictionClassif")
      )
    }
  ),
  active = list(
    predict_type = function(val) {
      "prob"
    }
  ),
  private = list(
    .train = function(inputs) {
      on.exit(lgr::get_logger("mlr3")$set_threshold("info"))

      lgr::get_logger("mlr3")$set_threshold("warn")

      task = inputs[[1]]
      positive = task$positive

      if (is.null(self$rr)) {
        rr = resample(task, self$learner, self$rsmp, store_models = TRUE)
      } else {
        rr = self$rr
      }

      self$learners = rr$learners
      preds = rr$predictions(predict_sets = "test")

      for (pred in preds) {
        pred_data = as.data.table(pred)
        calibration_data = data.table(truth = pred_data$truth,
                                      response = with(pred_data, get(paste0("prob.", positive))))

        colnames(calibration_data) = c("truth", "response")
        calibration_data$response = as.numeric(calibration_data$response)

        if (self$method == "platt") {
          task_for_calibrator = as_task_classif(calibration_data, target = "truth",
                                                positive = positive, id = "Task_cal")
          calibrator = lrn("classif.log_reg", predict_type = "prob")
          calibrator$train(task_for_calibrator)
          self$calibrators[[length(self$calibrators) + 1]] = calibrator
        } else if (self$method == "isotonic") {
          calibration_data$truth <- ifelse(calibration_data$truth == positive, 1, 0)
          calibrator = as.stepfun(stats::isoreg(x = calibration_data$response,
                                                y = calibration_data$truth))
          self$calibrators[[length(self$calibrators) + 1]] = calibrator
        } else if (self$method == "beta") {
          calibration_data$truth <- ifelse(calibration_data$truth == positive, 1, 0)
          calibrator = betacal::beta_calibration(p = calibration_data$response,
                                                 y = calibration_data$truth,
                                                 parameters = self$parameters)
          self$calibrators[[length(self$calibrators) + 1]] = calibrator
        }
      }
      return(list(NULL))
    },

    .predict = function(inputs) {
      task = inputs[[1]]
      positive = task$positive
      predictions = list()
      for (learner_index in seq_along(self$learners)) {
        learner = self$learners[[learner_index]]
        pred = learner$predict(task)
        pred_data = as.data.table(pred)
        calibration_data = data.table(truth = task$truth(),
                                      response = with(pred_data, get(paste0("prob.", positive))))
        colnames(calibration_data) = c("truth", "response")
        calibration_data$response = as.numeric(calibration_data$response)

        if (self$method == "platt") {
          task_for_calibrator = as_task_classif(calibration_data, target = "truth",
                                                positive = positive, id = "Task_cal")
          pred_calibrated = self$calibrators[[learner_index]]$predict(task_for_calibrator)
        } else if (self$method == "isotonic") {
          pred_calibrated = self$calibrators[[learner_index]](calibration_data$response)
          prob = as.matrix(data.frame(pred_calibrated, 1 - pred_calibrated))
          colnames(prob) = c(task$positive, task$negative)
          response = ifelse(pred_calibrated < 0.5, task$negative, task$positive)
          pred_calibrated = PredictionClassif$new(
            task = task,
            row_ids = task$row_ids,
            truth = task$truth(),
            prob = prob,
            response = response
          )
        } else if (self$method == "beta") {
          pred_calibrated = betacal::beta_predict(calibration_data$response,
                                                  self$calibrators[[learner_index]])
          prob = as.matrix(data.frame(pred_calibrated, 1 - pred_calibrated))
          colnames(prob) = c(task$positive, task$negative)
          response = ifelse(pred_calibrated < 0.5, task$negative, task$positive)
          pred_calibrated = PredictionClassif$new(
            task = task,
            row_ids = task$row_ids,
            truth = task$truth(),
            prob = prob,
            response = response
          )
        }
        predictions[[length(predictions) + 1]] = as.data.table(pred_calibrated)
      }

      response = rowMeans(sapply(predictions, function(x) as.numeric(x$response)))
      response = ifelse(response < 1.5, positive, task$negative)
      prob = as.matrix(data.frame(
        rowMeans(sapply(predictions, function(x) with(x, get(paste0("prob.", positive))))),
        rowMeans(sapply(predictions, function(x) with(x, get(paste0("prob.", task$negative)))))
      ))
      colnames(prob) = c(task$positive, task$negative)
      pred_calibrated = PredictionClassif$new(
        task = task,
        row_ids = task$row_ids,
        truth = task$truth(),
        prob = prob,
        response = response
      )
      return(list(pred_calibrated))
    },

    .additional_phash_input = function() {
      list(self$learner$hash)
    }
  )
)

#' @rdname mlr_pipeops_calibration
#' @export
mlr_pipeops$add("calibration", PipeOpCalibration)

