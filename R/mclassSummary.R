#' @name mclassSummary
#' @aliases mclassSummary
#' 
#' @title Classification error, cross-entropy (or logLoss), and Brier 
#' score performance measures across resamples
#' 
#' @description Calculates classification error, cross-entropy, and 
#' Brier score measures for evaluating a multi-class classifier output quality.  
#' This function can be used in \code{\link[caret]{train}()} function for 
#' selecting the hyperparameter(s) of a classifier. This can be achieved by
#' specifying the argument \code{metric} with \code{maximize = FALSE} in
#' \code{train()} function call, and \code{summaryFunction = mclassSummary}
#' with \code{classProbs = TRUE} in \code{\link[caret]{trainControl}}. 
#' See examples below.
#'
#' @param data	a data frame with columns \code{obs} and \code{pred} for the 
#' observed and predicted outcomes, and \code{prob} for predicted probabilities 
#' for each class. 
#' See the \code{classProbs} argument to \code{\link[caret]{trainControl}}.
#' @param lev a character vector of factors levels for the response. 
#' @param model	a character string for the model name (as taken from the 
#' \code{method} argument of \code{\link[caret]{train}}.
#' @param \dots additional arguments to be passed to the low level functions.
#'
#' @return A vector containing the metrics.
#'
#' @seealso \code{\link[caret]{twoClassSummary}}, \code{\link{fscoreSummary}}
#' 
#' @examples
#' \dontrun{
#' data = mlbench::mlbench.waveform(300)
#' data = data.frame(Class = data$classes, data$x)
#' data$Class = paste0("Class", data$Class)
#' describe(data)
#' 
#' mod1 = train(Class ~ . , data = data,
#'              method = "rpart2",
#'              tuneGrid = expand.grid(maxdepth = 1:10),
#'              metric = "CrossEntropy",
#'              maximize = FALSE,
#'              trControl = trainControl(method = "cv", number = 10,
#'                                       classProbs = TRUE,
#'                                       summaryFunction = mclassSummary,
#'                                       selectionFunction = "best") )
#' mod1
#' ggplot(mod1) +
#'   scale_x_continuous(breaks = mod1$results$maxdepth) +
#'   geom_errorbar(aes(ymin = with(mod1$results, CrossEntropy - CrossEntropySD/sqrt(10)),
#'                     ymax = with(mod1$results, CrossEntropy + CrossEntropySD/sqrt(10))),
#'                 width = 0.3)
#'                 
#' mod2 = train(Class ~ . , data = data,
#'              method = "rpart2",
#'              tuneGrid = expand.grid(maxdepth = 1:10),
#'              metric = "BrierScore",
#'              maximize = FALSE,
#'              trControl = trainControl(method = "cv", number = 10,
#'                                       classProbs = TRUE,
#'                                       summaryFunction = mclassSummary,
#'                                       selectionFunction = "best") )
#' mod2
#' ggplot(mod2) +
#'   scale_x_continuous(breaks = mod2$results$maxdepth) +
#'   geom_errorbar(aes(ymin = with(mod2$results, BrierScore - BrierScoreSD/sqrt(10)),
#'                     ymax = with(mod2$results, BrierScore + BrierScoreSD/sqrt(10))),
#'                 width = 0.3)
#' }
#' @importFrom ModelMetrics ce mlogLoss brier
#' @export

mclassSummary <- function(data, lev = NULL, model = NULL, ...) 
{
  requireNamespace("ModelMetrics")
  if(!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("Levels of observed and predicted data do not match.", call. = FALSE)
  has_class_probs <- all(lev %in% colnames(data))
  if(!has_class_probs)
    stop("Models must provide class probabilities.", call. = FALSE)

  obs <- do.call("cbind", lapply(levels(data[, "obs"]), 
                               function(x) ifelse(data[,"obs"] == x, 1, 0)))
  prob <- do.call("cbind", lapply(levels(data[, "pred"]), 
                                  function(x) data[, x]))
  c(ClassError = ModelMetrics::ce(actual = data$obs, 
                                  predicted = data$pred),
    CrossEntropy = ModelMetrics::mlogLoss(actual = data$obs, 
                                     predicted = prob),
    BrierScore = ModelMetrics::brier(actual = obs, 
                                     predicted = prob))
}
