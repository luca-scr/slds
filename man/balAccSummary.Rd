% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/balAccSummary.R
\name{balAccSummary}
\alias{balAccSummary}
\title{Balance Accuracy-Sensitivity-Specificity performance measures
across resamples}
\usage{
balAccSummary(data, lev = NULL, model = NULL, ...)
}
\arguments{
\item{data}{a data frame with columns \code{obs} and \code{pred} for the 
observed and predicted outcomes, and \code{prob} for predicted probabilities 
for each class. 
See the \code{classProbs} argument to \code{\link[caret]{trainControl}}.}

\item{lev}{a character vector of factors levels for the response.}

\item{model}{a character string for the model name (as taken from the 
\code{method} argument of \code{\link[caret]{train}}.}

\item{\dots}{additional arguments to be passed to the low level functions.}
}
\value{
A vector containing the metrics.
}
\description{
Calculates diagnostic-based measures
(Sensitivity, Specificity, and their average) for evaluating a 
two-class classifier output quality.

This function can be used in \code{\link[caret]{train}()} for
selecting the hyperparameter(s) of a classifier.  
This can be achieved by specifying the argument \code{metric} in 
\code{train()} function call, and the argument
\code{summaryFunction = balAccSummary} in 
\code{\link[caret]{trainControl}()}. 
See examples below.
}
\examples{
data = caret::twoClassSim(200)
describe(data)

mod = train(Class ~ . , data = data,
             method = "rpart2",
             tuneGrid = expand.grid(maxdepth = 1:10),
             metric = "BalancedAccuracy",
             trControl = trainControl(method = "cv", number = 10,
                                      # classProbs = TRUE,
                                      # savePredictions = TRUE,
                                      summaryFunction = balAccSummary,
                                      selectionFunction = "oneSE"))
mod
out = as.data.table(mod$results)[,.(maxdepth, BalancedAccuracy, BalancedAccuracySD#' )]
out[, lower := BalancedAccuracy - BalancedAccuracySD/sqrt(10)]
out[, upper := BalancedAccuracy + BalancedAccuracySD/sqrt(10)]
ggplot(mod, highlight = TRUE) + 
  geom_errorbar(data = out, aes(ymin = lower, ymax = upper))

}
\seealso{
\code{\link[caret]{twoClassSummary}},
         \code{\link{mclassSummary}}
         \code{\link{fscoreSummary}}
}
