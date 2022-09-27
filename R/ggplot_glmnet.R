#' @name ggplot.glmnet
#' @aliases ggplot.glmnet
#' 
#' @title ggplot coefficients from a 'glmnet' object
#' 
#' @description ggplot2 version of function \code{plot.glmnet()} available in package 'glmnet'

#' @param object fitted 'glmnet' model
#' @param xvar a character vector specifying the x-axis: \code{"norm"} plots against the L1-norm of the coefficients, \code{"lambda"} against the log-lambda sequence, and \code{"dev"} against the percent deviance explained.
#' @param lty linetype specification (recycling if necessary)
#' @param col color specification (recycling if necessary)
#' @param label If \code{TRUE}, label the curves with variable sequence numbers.
#' @param \dots additional arguments
#'
#' @seealso \link[glmnet]{plot.glmnet} 
#' 
#' @examples
#' 
#' X = matrix(rnorm(100 * 20), 100, 20)
#' b = c(runif(5, min = -1, max = 1), rep(0,15))
#' y = X %*% b + rnorm(100)
#' fit = glmnet(X,y)
#' ggplot(fit)
#' ggplot(fit, xvar = "lambda", col = palette(), label = TRUE)
#' ggplot(fit, xvar = "dev", col = palette()[1:4], lty = 1:3)
#' 
#' @importFrom ggplot2 ggplot scale_x_continuous geom_text labs
#' 
#' @export

ggplot.glmnet <- function(object, 
                          xvar = c("norm", "lambda", "dev"), 
                          lty = 1, col = 1,
                          label = FALSE, ...)
{
  lambda = object$lambda
  df = object$df
  dev = object$dev.ratio

  ## beta should be in "dgCMatrix" format
  beta = object$beta
  which = glmnet:::nonzeroCoef(beta)
  nwhich = length(which)
  switch(nwhich+1, # we add one to make switch work
         "0" = {
           warning("No plot produced since all coefficients zero")
           return()
         },
         "1" = warning("1 or less nonzero coefficients; glmnet plot is not meaningful")
  )
  beta = as.matrix(beta[which,,drop=FALSE])
  
  xvar = match.arg(xvar)
  switch(xvar,
    "norm" = {
      index = apply(abs(beta),2,sum)
              # if(missing(norm)) apply(abs(beta),2,sum) else norm
      iname = "L1 Norm"
      approx.f = 1
    },
    "lambda" = {
      index = log(lambda)
      iname = "Log Lambda"
      approx.f = 0
    },
    "dev"= {
      index = dev
      iname = "Fraction Deviance Explained"
      approx.f = 1
    }
  )

  plot = ggmatplot(index, t(beta), type="l", 
                   lty = lty, col = col, ...) +
    labs(x = iname, y = "Coefficients")
  
  # compute df by interpolating to df at next smaller lambda
  # thanks to Yunyang Qian
  atdf = pretty(index)
  prettydf = approx(x = index, y = df, xout = atdf, rule = 2,
                    method = "constant", f = approx.f)$y
  plot = plot +
    scale_x_continuous(
      sec.axis = sec_axis(~ .*1, 
                          name = expression("Number of coefs" != "0"),
                          breaks = atdf, labels = prettydf))
  
 if(label)
 {
   nnz = length(which)
   if(xvar=="lambda")
   {
     xpos = min(index)
     just = -1
   } else
   {
     xpos = max(index)
     just = 1
   }
   plot = plot + 
     geom_text(aes(x = rep(xpos, nnz), 
                   y = beta[,ncol(beta)]),
               size = 8*0.36,
               label = paste(which), 
               hjust = if(just==1) "right" else "left", 
               nudge_x = just*diff(range(index))*0.03)
 }

  return(plot)
}

