% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PipeOpCalibration.R
\name{PipeOpCalibration}
\alias{PipeOpCalibration}
\title{Calibration Pipeline Operator}
\description{
Pipeline operator for calibrating classification learner using different calibration methods.
Supports Platt scaling, isotonic regression, and beta calibration.
}
\examples{
# Example usage
set.seed(1)
library(mlr3verse)

# Load the task
data("Sonar", package = "mlbench")
task = as_task_classif(Sonar, target = "Class", positive = "M")

# Initialize the base learner
learner_uncal <- lrn("classif.ranger", predict_type = "prob")

# Initialize the calibrated learner
rsmp <- rsmp("cv", folds = 5)
learner_cal <- as_learner(PipeOpCalibration$new(learner = learner_uncal,
                                                method = "platt",
                                                rsmp = rsmp))

# Set ID's for the learners
learner_cal$id <- "Calibrated Learner"

# Train the calibrated learner
learner_cal$train(task)
}
\references{
Filho TMS, Kull M (2017). betacal: Beta Calibration_. R package version 0.1.0, \url{https://CRAN.R-project.org/package=betacal}.
}
\section{Super class}{
\code{\link[mlr3pipelines:PipeOp]{mlr3pipelines::PipeOp}} -> \code{PipeOpCalibration}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{learner}}{\code{\link[mlr3:Learner]{Learner}}\cr Base learner to be calibrated.}

\item{\code{method}}{\code{character(1)}\cr Calibration method used.}

\item{\code{rsmp}}{\code{\link[mlr3:Resampling]{Resampling}}\cr Resampling strategy.}

\item{\code{learners}}{\code{list}\cr List of learners obtained from resampling.}

\item{\code{calibrators}}{\code{list}\cr List of calibrator models.}

\item{\code{rr}}{\code{\link[mlr3:ResampleResult]{ResampleResult}}\cr Resample result object.}

\item{\code{parameters}}{\code{character(1)}\cr Parameters for beta calibration.}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{predict_type}}{\code{character(1)}\cr Set predict_type to \code{"prob"}.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PipeOpCalibration-new}{\code{PipeOpCalibration$new()}}
\item \href{#method-PipeOpCalibration-clone}{\code{PipeOpCalibration$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="mlr3pipelines" data-topic="PipeOp" data-id="help"><a href='../../mlr3pipelines/html/PipeOp.html#method-PipeOp-help'><code>mlr3pipelines::PipeOp$help()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3pipelines" data-topic="PipeOp" data-id="predict"><a href='../../mlr3pipelines/html/PipeOp.html#method-PipeOp-predict'><code>mlr3pipelines::PipeOp$predict()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3pipelines" data-topic="PipeOp" data-id="print"><a href='../../mlr3pipelines/html/PipeOp.html#method-PipeOp-print'><code>mlr3pipelines::PipeOp$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3pipelines" data-topic="PipeOp" data-id="train"><a href='../../mlr3pipelines/html/PipeOp.html#method-PipeOp-train'><code>mlr3pipelines::PipeOp$train()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PipeOpCalibration-new"></a>}}
\if{latex}{\out{\hypertarget{method-PipeOpCalibration-new}{}}}
\subsection{Method \code{new()}}{
Creates a new \code{PipeOpCalibration} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PipeOpCalibration$new(
  learner = NULL,
  method = "platt",
  rsmp = NULL,
  rr = NULL,
  parameters = "abm",
  param_vals = list()
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{learner}}{Base learner to be calibrated. predict_type has to be \code{"prob"}.}

\item{\code{method}}{Calibration method to use. One of \code{"platt"}, \code{"isotonic"}, or \code{"beta"}. Default is \code{"platt"}.}

\item{\code{rsmp}}{Resampling strategy for cross-validation. Default is \code{rsmp("cv", folds = 5)}.}

\item{\code{rr}}{Resample result object, if provided.}

\item{\code{parameters}}{Parameters for beta calibration. Default is \code{"abm"}.}

\item{\code{param_vals}}{param_vals, copied from base learner}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PipeOpCalibration-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PipeOpCalibration-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PipeOpCalibration$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
