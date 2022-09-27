#' @name plotImage
#' @aliases plotImage
#' 
#' @title Plot image data
#' 
#' @description Plot a matrix representing a Grey-scale image or a 
#' 3-dimensional array representing a RGB image.
#' 
#' @param x a matrix of data values for Grey-scale images and a 
#' 3-dimensional array for RGB images.
#' @param max the maximum value of Grey/RGB colour scale (default 255 
#' for integer values in the range [0, 255]; another common value is 1 
#' for values in the range [0, 1].
#' @param mar the margin used in the base R graphic. 
#' @param restore.par logical, if TRUE the previous par() of graphical 
#' window are restored.
#' @param \dots additional arguments to be passed to the low level 
#' functions.
#' 
#' @importFrom graphics par rasterImage
#' @importFrom grDevices as.raster
#' 
#' @rdname plotImage
#' @export

plotImage <- function(x, max = 255, mar = c(0,0,0,0), 
                      restore.par = TRUE, ...)
{
  x[x < 0] <- 0
  x[x > max] <- max
  x <- as.raster(x, max = max)
  h <- nrow(x)
  w <- ncol(x)
  oldpar <- par(no.readonly = TRUE)
  if(restore.par) 
     on.exit(par(oldpar))
  par(mar = mar)
  plot(0, 0, type = "n", xlim = c(1, w), ylim = c(1, h),
       axes = FALSE, ann = FALSE, asp = 1)
  rasterImage(x, 1, 1, w, h, ...)
}
