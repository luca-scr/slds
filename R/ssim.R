#' @name ssim
#' @aliases ssim
#' 
#' @title Structural similarity index 
#' 
#' @description Computes the Structural Similarity Index Measure (SSIM) is a
#' widely adopted index for measuring the similarity between two images by
#' evaluating the perceived change in structural information.
#' SSIM \eqn{\in}{in} [0,1], with SSIM = 1 indicating a perfect match with
#' the original image. For values larger than 0.95 two images are
#' considered indistinguishable to the human eye.
#' 
#' @param x = a matrix of values for a segmented/approximated image
#' @param y = a matrix of values for the original image
#' @param L = dynamic range of the pixel-values, for a 8-bit image 
#'            $L = 2^8-1 = 255$.
#'
#' @return The computed SSIM value.
#'
#' @examples
#' 
#' @export

ssim <- function(x, y, L = 255)
{
# Structural similarity index measure (SSIM) is used for measuring the
# similarity between two images.
#
# x = a matrix/array of the original image
# y = a matrix/array of the segmented/approximated image
# L = dynamic range of the pixel-values, typically this is equal to 
#     L = 2^{#bits per pixel −1}, so for a 8-bit image L = 2^8-1 = 255
  
  x <- as.array(x)
  if(min(x) >= 0 & max(x) <= 1) x <- x*L
  if(!(min(x) >= 0 & max(x) <= L)) 
  {
  	warning("x values forced in the [0,L] range!")
  	x <- array(pmin(pmax(0, x), L), dim = dim(x))		
  }
  y <- as.array(y)
  if(min(y) >= 0 & max(y) <= 1) y <- y*L
  if(!(min(y) >= 0 & max(y) <= L)) 
  {
  	warning("y values forced in the [0,L] range!")
  	y <- array(pmin(pmax(0, y), L), dim = dim(y))
  }
  if(length(x) != length(y))
    step("dimension of x and y do not match!")
  #
  k1 <- 0.01; k2 <- 0.03
  c1 <- (k1*L)^2; c2 <- (k2*L)^2
  mx <- apply(x, 3, mean)
  my <- apply(y, 3, mean)
  s2x <- apply(x, 3, function(x) var(as.vector(x)))
  s2y <- apply(y, 3, function(y) var(as.vector(y)))
  sxy <- sapply(1:3, function(j) cov(as.vector(x[,,j]), as.vector(y[,,j])))
  SSIM <- (2*mx*my + c1)*(2*sxy + c2)/(mx^2+my^2+c1)/(s2x+s2y+c2)
  return(mean(SSIM))
}
