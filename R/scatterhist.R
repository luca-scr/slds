#' @name scatterhist
#' @aliases scatterhist
#' 
#' @title Scatterplot with marginal histograms
#' 
#' @description Plot a scatterplot of bivariate data with histograms for the marginal distributions.
#'
#' @param x a vector, matrix or data frame
#' @param y if provided, a vector
#' @param \dots additional arguments to be passed to the low level functions.
#'
#' @return None, just produces a plot.
#'
#' @examples
#' 
#' x <- rnorm(200)
#' y <- 0.5*x + rnorm(200, 0, 0.1)
#' scatterhist(x, y)
#' 
#' @importFrom graphics barplot layout
#' @importFrom grDevices xy.coords
#' 
#' @export scatterhist

scatterhist <- function(x, y = NULL, ...)
{
  xy <- xy.coords(x, y)
  # browser()
  if(is.null(xy$xlab)) xy$xlab <- deparse(substitute(x))
  if(is.null(xy$ylab)) xy$ylab <- deparse(substitute(y))
  args <- list(...)
  argshist <- args[names(args) %in% c("breaks")]
  if(is.null(argshist$breaks)) argshist$breaks <- "scott"

  xhist <- do.call("hist", c(list(xy$x), argshist, plot = FALSE))
  yhist <- do.call("hist", c(list(xy$y), argshist, plot = FALSE))
  top <- max(c(xhist$counts, yhist$counts))
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  layout(matrix(c(2,0,1,3), ncol=2, byrow=TRUE), 
         widths = c(4/5, 1/5), heights = c(1/5, 4/5))
  #
  par(mar=c(oldpar$mar[1:2],0.2,0.2))
  do.call("plot", c(xy, args))
  #
  par(mar=c(0,oldpar$mar[2],1,1))
  barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0, 
          border = FALSE, col = args$col)
  #
  par(mar=c(oldpar$mar[1],0,1,1))
  barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, 
          border = FALSE, col = args$col, horiz=TRUE)
}
