#' @name prcompRecon
#' @aliases prcompRecon
#' 
#' @title Principal components data reconstruction
#' 
#' @description Return the data matrix reconstructed using a subset of 
#' principal components.
#' 
#' @param object an object returned by `prcomp()` function. 
#' @param npcs the number of principal components to use in data reconstruction
#' @param pcs specify which principal components to use in data reconstruction. If provided it takes precedence over `npcs`
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
#' X = prcompRecon(PCA, pcs = c(1,4))
#' pairs(X, gap = 0)
#' 
#' @rdname prcompRecon
#' @export

prcompRecon <- function(object, npcs = 2, pcs = NULL, ...)
{
  stopifnot(inherits(object, "prcomp"))
  
  inc <- which(object$sdev > sqrt(.Machine$double.eps))
  pcs <- if(!is.null(pcs)) as.numeric(pcs) else as.numeric(1:npcs)
  pcs <- intersect(pcs, inc)
  Z <- object$x[,pcs,drop=FALSE]
  V <- object$rotation[,pcs,drop=FALSE]
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
#' @param data the original data matrix.
#' @param \dots additional arguments to be passed to the low level functions.
#' 
#' @return A data frame containing the reconstruction error (RE), the root mean square error (RMSE), and the R^2 (Rsq) for sequential principal components (pc).
#'
#' @examples
#' 
#' PCA = prcomp(iris[,1:4])
#' tab = prcompReconError(PCA, iris[,1:4])
#' zapsmall(tab,7)
#' 
#' @rdname prcompReconError
#' @export

prcompReconError <- function(object, data, ...)
{
  stopifnot(inherits(object, "prcomp"))
  data <- as.matrix(data)
  pcs <- which(object$sdev > sqrt(.Machine$double.eps))
	npcs <- length(pcs)
	RE <- rep(as.double(NA), npcs)
	for(q in 1:npcs)
	{
    RE[q] <- sum((data - prcompRecon(object, npcs = q))^2)
	}
	RMSE <- sqrt(RE/prod(dim(data)))
	Rsq <- 1 - RE/sum(scale(data, center = object$center, scale = FALSE)^2)

	out <- data.frame(pc = pcs, RE = RE, RMSE = RMSE, Rsq = Rsq)
  return(out)
}

tr <- function (x)
{
   x <- as.matrix(x)
   sum(diag(x))
}

