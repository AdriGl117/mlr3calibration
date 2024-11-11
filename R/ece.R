#' @title Expected Calibration Error
#'
#' @description
#' Calculates the Expected Calibration Error (ECE) for classification tasks.
#' ECE measures the difference between predicted probabilities and actual outcomes,
#' providing an assessment of how well the predicted probabilities are calibrated.
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
