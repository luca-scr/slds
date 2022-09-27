#' @name fscoreSummary
#' @aliases fscoreSummary
#' 
#' @title Precision-Recall-Fscore performance measures across resamples
#' 
#' @description Calculates information retrieval measures
#' (Precision, Recall, F1, F2) for evaluating a two-class classifier 
#' output quality.  
#' This function can be used in \code{\link[caret]{train}()} function for 
#' selecting the hyperparameter(s) of a classifier. This can be achieved by
#' specifying the argument \code{metric} in \code{train()} function call, and
#' \code{summaryFunction = fscoreSummary} and \code{classProbs = TRUE}
#' in \code{\link[caret]{trainControl}}. 
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
#' @seealso \code{\link[caret]{twoClassSummary}}, \code{\link{ceSummary}}
#' 
#' @examples
#' data = caret::twoClassSim(200)
#' describe(data)
#' 
#' mod1 = train(Class ~ . , data = data,
#'              method = "rpart2",
#'              tuneGrid = expand.grid(maxdepth = 1:10),
#'              metric = "F1",
#'              trControl = trainControl(method = "cv", number = 10,
#'                                       classProbs = TRUE,
#'                                       savePredictions = TRUE,
#'                                       summaryFunction = fscoreSummary,
#'                                       selectionFunction = "best") )
#' mod1
#' plot(mod1)
#' 
#' mod2 = train(Class ~ . , data = data,
#'              method = "rpart2",
#'              tuneGrid = expand.grid(maxdepth = 1:10),
#'              metric = "F2",
#'              trControl = trainControl(method = "cv", number = 10,
#'                                       classProbs = TRUE,
#'                                       summaryFunction = fscoreSummary,
#'                                       selectionFunction = "best") )
#' mod2
#' plot(mod2)
#' 
#' mod3 = train(Class ~ . , data = data,
#'              method = "rf",
#'              tuneGrid = expand.grid(mtry = 1:5),
#'              metric = "F1",
#'              trControl = trainControl(method = "cv", number = 10,
#'                                       classProbs = TRUE,
#'                                       summaryFunction = fscoreSummary,
#'                                       selectionFunction = "best") )
#' mod3
#' plot(mod3)
#' 
#' mod4 = train(Class ~ . , data = data,
#'              method = "rf",
#'              tuneGrid = expand.grid(mtry = 1:5),
#'              metric = "F2",
#'              trControl = trainControl(method = "cv", number = 10,
#'                                       classProbs = TRUE,
#'                                       summaryFunction = fscoreSummary,
#'                                       selectionFunction = "best") )
#' mod4
#' plot(mod4)
#' 
#' @importFrom ModelMetrics fScore precision recall
#' @export

fscoreSummary <- function(data, lev = NULL, model = NULL, ...) 
{
  requireNamespace("ModelMetrics")
  if(length(levels(data$obs)) > 2) 
    stop(paste("Your outcome has", length(levels(data$obs)), 
               "levels. `fscoreSummary`` function isn't appropriate."), 
         call. = FALSE)
  if(!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("Levels of observed and predicted data do not match.", 
         call. = FALSE)
  
  F1 <- try(ModelMetrics::fScore(actual = ifelse(data$obs == lev[1], 1, 0),
                                 predicted = data[, lev[1]], beta = 1, ...), 
            silent = TRUE)
  if(inherits(F1, "try-error")) F1 <- NA
  
  F2 <- try(ModelMetrics::fScore(actual = ifelse(data$obs == lev[1], 1, 0),
                                 predicted = data[, lev[1]], beta = 2, ...), 
            silent = TRUE)
  if(inherits(F2, "try-error")) F2 <- NA
  
  c(Precision = ModelMetrics::precision(actual = ifelse(data$obs == lev[1], 1, 0),
                                        predicted = data[, lev[1]], ...), 
    Recall    = ModelMetrics::recall(actual = ifelse(data$obs == lev[1], 1, 0),
                                     predicted = data[, lev[1]], ...),
    F1        = F1,
    F2        = F2)
}
