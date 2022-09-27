#' @name prcompRecon
#' @aliases prcompRecon
#' 
#' @title Principal components data reconstruction
#' 
#' @description Return the data matrix reconstructed using a subset of 
#' principal components.
#' 
#' @param object an object returned by 'prcomp' function. 
#' @param npcs the number of principal componets to use in the reconstruction
#' @param \dots additional arguments to be passed to the low level functions.
#' 
#' @return The reconstructed data matrix. 
#'
#' @examples
#' 
#' pairs(iris[,1:4], gap = 0)
#' PCA = prcomp(iris[,1:4])
#' X = prcompRecon(PCA, 2)
#' pairs(X, gap = 0)
#' 
#' @rdname prcompRecon
#' @export

prcompRecon <- function(object, npcs = 2, ...)
{
  stopifnot(inherits(object, "prcomp"))
	npcs <- min(as.integer(npcs),
	            sum(object$sdev > sqrt(.Machine$double.eps)))
  Z <- object$x[,seq(npcs),drop=FALSE]
  V <- object$rotation[,seq(npcs),drop=FALSE]
  x <- Z %*% t(V)
  if(is.numeric(object$scale))
    x <- scale(x, center = FALSE, scale = 1/object$scale)
  if(is.numeric(object$center))
    x <- scale(x, center = -object$center, scale = FALSE)
  return(x)
}

#' @name prcompReconError
#' @aliases prcompReconError
#' 
#' @title Principal components reconstruction error
#' 
#' @description Return the squared reconstruction error from using a subset of 
#' principal components.
#' 
#' @param object an object returned by 'prcomp' function. 
#' @param data the original data matrix
#' @param \dots additional arguments to be passed to the low level functions.
#' 
#' @return A data frame containing the reconstruction error (RE), the mean
#' square error (MSE), and the R^2 for increasing value of principal 
#' components.
#'
#' @examples
#' 
#' PCA = prcomp(iris[,1:4])
#' df = prcompReconError(PCA, iris[,1:4])
#' zapsmall(df,5)
#' 
#' @rdname prcompReconError
#' @export

prcompReconError <- function(object, data, ...)
{
  stopifnot(inherits(object, "prcomp"))
  data <- as.matrix(data)
	npcs <- length(object$sdev)
	RE <- rep(as.double(NA), npcs)
	for(q in 1:npcs)
	{
    RE[q] <- sum((data - prcompRecon(object, npcs = q))^2)
	}
	MSE <- RE/prod(dim(data))
	Rsq <- 1 - RE/sum(scale(data, center = object$center, scale = FALSE)^2)

	out <- data.frame(npc = 1:npcs, RE = RE, MSE = MSE, Rsq = Rsq)
  return(out)
}

tr <- function (x)
{
   x <- as.matrix(x)
   sum(diag(x))
}

