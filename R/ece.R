#' @title Expected Calibration Error
#'
#' @description
#' Calculates the Expected Calibration Error (ECE) for classification tasks.
#' ECE measures the difference between predicted probabilities and actual outcomes,
#' providing an assessment of how well the predicted probabilities are calibrated.
#'
#' @references
#' Schwarz J, Heider D (2019). “GUESS: Projecting Machine Learning Scores to Well-Calibrated Probability Estimates for Clinical Decision Making.” _Bioinformatics_, *35*(14), 2458-2465.
#'
#' @examples
#' # Example usage
#' set.seed(1)
#' library(mlr3verse)
#'
#' # Load the task
#' data("Sonar", package = "mlbench")
#' task = as_task_classif(Sonar, target = "Class", positive = "M")
#' splits = partition(task)
#' task_train = task$clone()$filter(splits$train)
#' task_test = task$clone()$filter(splits$test)
#'
#' # Initialize the base learner
#' learner_uncal <- lrn("classif.ranger", predict_type = "prob")
#'
#' # Initialize the calibrated learner
#' rsmp <- rsmp("cv", folds = 5)
#' learner_cal <- as_learner(PipeOpCalibration$new(learner = learner_uncal,
#'                                                 method = "platt",
#'                                                 rsmp = rsmp))
#'
#' # Set ID's for the learners
#' learner_cal$id <- "Calibrated Learner"
#'
#' # Train the calibrated learner
#' learner_cal$train(task_train)
#'
#' # Predict the learner
#' prediction <- learner_cal$predict(task_test)
#'
#' # Calculate the ECE
#' ece <- prediction$score(ece$new())
#'
#' @export
ece = R6::R6Class("ece",
                  inherit = mlr3::MeasureClassif,
                  public = list(
                    #' @description
                    #' Creates a new `ece` object.
                    initialize = function() {
                      super$initialize(
                        id = "classif.ece",
                        packages = "CalibratR",
                        properties = character(),
                        predict_type = "prob",
                        range = c(0, 1),
                        minimize = TRUE
                      )
                    }
                  ),
                  private = list(
                    .score = function(prediction, ...) {
                      actual = ifelse(prediction$truth == colnames(prediction$prob)[1], 1, 0)
                      predicted = prediction$prob[, 1]
                      CalibratR::getECE(actual = actual, predicted = predicted)
                    }
                  )
)

mlr3::mlr_measures$add("classif.ece", ece)
