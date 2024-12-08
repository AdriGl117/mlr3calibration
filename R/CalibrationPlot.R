#' @title Calibration Plot Function
#'
#' @name calibrationplot
#'
#' @description
#' Generates calibration plots, also known as reliability curves, for one or more classification learners.
#' This function evaluates how well the predicted probabilities from the learners are calibrated with the true outcomes.
#'
#' @param learners `list` of [`Learner`][mlr3::Learner]\cr
#'   List of trained learners to be evaluated.
#' @param task [`Task`][mlr3::Task]\cr
#'   Task containing the data to be used for prediction and evaluation.
#' @param bins `integer(1)`\cr
#'   Number of bins to use when grouping predicted probabilities. Default is `11`.
#' @param smooth `logical(1)`\cr
#'   Whether to plot a smoothed calibration curve using LOESS. Default is `FALSE`.
#' @param CI `logical(1)`\cr
#'   Whether to include confidence intervals when `smooth` is `TRUE`. Default is `FALSE`.
#' @param rug `logical(1)`\cr
#'   Whether to add a rug plot to show individual prediction points. Default is `FALSE`.
#'
#' @return A `ggplot` object displaying the reliability curves.
#'
#' @importFrom dplyr group_by summarise first
#' @importFrom ggplot2 ggplot aes geom_line geom_smooth geom_point labs
#' @importFrom ggplot2 scale_color_manual theme_minimal theme element_rect
#' @importFrom ggplot2 xlim ylim
#' @importFrom stats setNames
#' @importFrom magrittr %>%
#' @importFrom utils globalVariables
#'
#' @details
#' For each learner, the function predicts probabilities on the given task.
#' The predicted probabilities are divided into bins, and within each bin,
#' the mean predicted probability and the mean observed outcome are calculated.
#' These values are then plotted to assess calibration.
#'
#' @references
#' Niculescu-Mizil, A., & Caruana, R. (2005). Predicting good probabilities with supervised learning. In Proceedings of the 22nd international conference on Machine learning (pp. 625-632).
#'
#' @family Plotting Functions
#'
#' @examples
#' # Example usage of calibrationplot
#' set.seed(1)
#' data("Sonar", package = "mlbench")
#' task = as_task_classif(Sonar, target = "Class", positive = "M")
#' splits = partition(task)
#' task_train = task$clone()$filter(splits$train)
#' task_test = task$clone()$filter(splits$test)
#' # Initialize the uncalibrated learner
#' learner_uncal <- lrn("classif.xgboost", nrounds = 50, predict_type = "prob")
#'
#' # Initialize the calibrated learner
#' rsmp <- rsmp("cv", folds = 5)
#' learner_cal <- as_learner(PipeOpCalibration$new(learner = learner_uncal,
#'                                                 method = "beta",
#'                                                 rsmp = rsmp))
#'
#' # Set ID's for the learners
#' learner_uncal$id <- "Uncalibrated Learner"
#' learner_cal$id <- "Calibrated Learner"
#'
#' # Train the learners
#' learner_uncal$train(task_train)
#' learner_cal$train(task_train)
#'
#' # List the Learners you want to plot
#' lrns = list(learner_uncal, learner_cal)
#'
#' # Plot the reliability curve
#' calibrationplot(lrns, task_test, smooth = TRUE)
#'
#' @export

utils::globalVariables(c("bin", "mean_res", "mean_truth", "learner_id"))

calibrationplot <- function(learners, task, bins = 10,
                            smooth = FALSE, CI = FALSE, rug = FALSE) {

  all_data <- data.frame()
  positive <- task$positive

  for (learner in learners) {
    prediction <- learner$predict(task)
    res <- prediction$prob[, 1]
    truth <- ifelse(prediction$truth == positive, 1, 0)
    data <- data.frame(res, truth, learner_id = learner$id)
    data <- data[order(data$res), ]
    data$bin <- cut(data$res, breaks = seq(0, 1, length.out = bins + 1),
                    include.lowest = TRUE)
    data <- data %>% group_by(bin) %>% summarise(mean_res = mean(res),
              mean_truth = mean(truth), learner_id = first(learner_id))
    all_data <- rbind(all_data, data)
  }

  dummy_line <- data.frame(mean_res = c(0, 1), mean_truth = c(0, 1),
                           learner_id = "Perfectly Calibrated")

  p <- ggplot() +
    geom_line(data = dummy_line, aes(x = mean_res, y = mean_truth,
      color = learner_id), linetype = "dashed", show.legend = TRUE) +
    theme_minimal() +
    xlim(0, 1) +
    ylim(0, 1) +
    labs(x = "Mean Prediction", y = "Mean Truth", color = "Learner") +
    scale_color_manual(values = c("Perfectly Calibrated" = "black",
      stats::setNames(scales::hue_pal()(length(unique(all_data$learner_id))),
        unique(all_data$learner_id)))) +
    theme(legend.position = c(0.85, 0.25)) +
    theme(legend.background = element_rect(color = "black", size = 0.5)) +
    ggtitle("Reliability Curve") +
    theme(plot.title = element_text(hjust = 0.5, size = 20))

  if (smooth) {
    p <- p + geom_smooth(data = all_data, aes(x = mean_res,
          y = mean_truth, color = learner_id), method = "loess", se = CI)
  } else {
    p <- p + geom_point(data = all_data, aes(x = mean_res,
              y = mean_truth, color = learner_id)) +
      geom_line(data = all_data, aes(x = mean_res,
                                     y = mean_truth, color = learner_id))
  }

  return(p)
}
