#' @name balAccSummary
#' @aliases balAccSummary
#' 
#' @title Balance Accuracy-Sensitivity-Specificity performance measures
#' across resamples
#' 
#' @description Calculates diagnostic-based measures
#' (Sensitivity, Specificity, and their average) for evaluating a 
#' two-class classifier output quality.
#' 
#' This function can be used in \code{\link[caret]{train}()} for
#' selecting the hyperparameter(s) of a classifier.  
#' This can be achieved by specifying the argument \code{metric} in 
#' \code{train()} function call, and the argument
#' \code{summaryFunction = balAccSummary} in 
#' \code{\link[caret]{trainControl}()}. 
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
#' @seealso \code{\link[caret]{twoClassSummary}},
#'          \code{\link{mclassSummary}}
#'          \code{\link{fscoreSummary}}
#' 
#' @examples
#' data = caret::twoClassSim(200)
#' describe(data)
#' 
#' mod = train(Class ~ . , data = data,
#'              method = "rpart2",
#'              tuneGrid = expand.grid(maxdepth = 1:10),
#'              metric = "BalancedAccuracy",
#'              trControl = trainControl(method = "cv", number = 10,
#'                                       # classProbs = TRUE,
#'                                       # savePredictions = TRUE,
#'                                       summaryFunction = balAccSummary,
#'                                       selectionFunction = "oneSE"))
#' mod
#' out = as.data.table(mod$results)[,.(maxdepth, BalancedAccuracy, BalancedAccuracySD#' )]
#' out[, lower := BalancedAccuracy - BalancedAccuracySD/sqrt(10)]
#' out[, upper := BalancedAccuracy + BalancedAccuracySD/sqrt(10)]
#' ggplot(mod, highlight = TRUE) + 
#'   geom_errorbar(data = out, aes(ymin = lower, ymax = upper))
#' 
#' @export

balAccSummary <- function(data, lev = NULL, model = NULL, ...) 
{
  if(length(levels(data$obs)) > 2) 
    stop(paste("Your outcome has", length(levels(data$obs)), 
               "levels. `fscoreSummary`` function isn't appropriate."), 
         call. = FALSE)
  if(!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("Levels of observed and predicted data do not match.", 
         call. = FALSE)
  
  Sens <- try(caret::sensitivity(data[, "pred"], data[, "obs"], 
                                 positive = lev[1]))
  if(inherits(Sens, "try-error")) Sens <- NA
  
  Spec <- try(caret::specificity(data[, "pred"], data[, "obs"], 
                                 negative = lev[2]))
  if(inherits(Spec, "try-error")) Spec <- NA
  
  out <- c("BalancedAccuracy" = (Sens+Spec)/2, 
           "Sensitivity" = Sens, "Specificity" = Spec)
  return(out)
}
