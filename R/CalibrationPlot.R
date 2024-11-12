#' @title Calibration Plot Function
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
#' @return A `ggplot` object displaying the calibration plot(s).
#'
#' @import checkmate dplyr ggplot2
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
#' @export
#'
#' @examples
#' # Example usage of calibrationplot
#' library(ggplot2)
#' # List the learners you want to plot
#' lrns = list(learner_uncal, learner_cal)
#' # Plot the reliability curves
#' calibrationplot(lrns, task_test, smooth = TRUE)
calibrationplot <- function(learners, task, bins = 10,
                            smooth = FALSE, CI = FALSE, rug = FALSE) {

  if (!require("dplyr")) {
    stop("The 'dplyr' package is required to use this function. Please install the libray and try it again")
  }

  if (!require("ggplot2")) {
    stop("The 'ggplot2' package is required to use this function. Please install the libray and try it again")
  }

  all_data <- data.frame()
  positive <- task$positive

  for (learner in learners) {
    prediction <- learner$predict(task)
    res <- prediction$prob[, 1]
    truth <- ifelse(prediction$truth == positive, 1, 0)
    data <- data.frame(res, truth, learner_id = learner$id)
    data <- data[order(data$res), ]
    data$bin <- cut(data$res, breaks = seq(0, 1, length.out = bins + 1), include.lowest = TRUE)
    data <- data %>% group_by(bin) %>% summarise(mean_res = mean(res), mean_truth = mean(truth), learner_id = first(learner_id))
    all_data <- rbind(all_data, data)
  }

  dummy_line <- data.frame(mean_res = c(0, 1), mean_truth = c(0, 1), learner_id = "Perfectly Calibrated")

  p <- ggplot2::ggplot() +
    geom_line(data = dummy_line, aes(x = mean_res, y = mean_truth, color = learner_id), linetype = "dashed", show.legend = TRUE) +
    theme_minimal() +
    xlim(0, 1) +
    ylim(0, 1) +
    labs(x = "Mean Prediction", y = "Mean Truth", color = "Learner") +
    scale_color_manual(values = c("Perfectly Calibrated" = "black", setNames(scales::hue_pal()(length(unique(all_data$learner_id))), unique(all_data$learner_id)))) +
    theme(legend.position = c(0.85, 0.25)) +
    theme(legend.background = element_rect(color = "black", size = 0.5)) +
    ggtitle("Reliability Curve") +
    theme(plot.title = element_text(hjust = 0.5, size = 20))

  if (smooth) {
    p <- p + geom_smooth(data = all_data, aes(x = mean_res, y = mean_truth, color = learner_id), method = "loess", se = CI)
  } else {
    p <- p + geom_point(data = all_data, aes(x = mean_res, y = mean_truth, color = learner_id)) +
      geom_line(data = all_data, aes(x = mean_res, y = mean_truth, color = learner_id))
  }

  return(p)
}
