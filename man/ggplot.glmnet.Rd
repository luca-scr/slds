% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot_glmnet.R
\name{ggplot.glmnet}
\alias{ggplot.glmnet}
\title{ggplot coefficients from a 'glmnet' object}
\usage{
\method{ggplot}{glmnet}(
  object,
  xvar = c("norm", "lambda", "dev"),
  lty = 1,
  col = 1,
  label = FALSE,
  ...
)
}
\arguments{
\item{object}{fitted 'glmnet' model}

\item{xvar}{a character vector specifying the x-axis: \code{"norm"} plots against the L1-norm of the coefficients, \code{"lambda"} against the log-lambda sequence, and \code{"dev"} against the percent deviance explained.}

\item{lty}{linetype specification (recycling if necessary)}

\item{col}{color specification (recycling if necessary)}

\item{label}{If \code{TRUE}, label the curves with variable sequence numbers.}

\item{\dots}{additional arguments}
}
\description{
ggplot2 version of function \code{plot.glmnet()} available in package 'glmnet'
}
\examples{

X = matrix(rnorm(100 * 20), 100, 20)
b = c(runif(5, min = -1, max = 1), rep(0,15))
y = X \%*\% b + rnorm(100)
fit = glmnet(X,y)
ggplot(fit)
ggplot(fit, xvar = "lambda", col = palette(), label = TRUE)
ggplot(fit, xvar = "dev", col = palette()[1:4], lty = 1:3)

}
\seealso{
\link[glmnet]{plot.glmnet}
}
