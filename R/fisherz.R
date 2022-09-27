#' @name FisherZ
#' @aliases FisherZinv
#' 
#' @title Fisher Z transformation
#' 
#' @description Computes the Fisher Z-transform and its inverse.
#' 
#' @param rho = the value of the correlation in \eqn{[0,1]}
#' @param z = the value of the z-transform in \eqn{(-\infty, +\infty)}
#'
#' @return The transformed or back-transformed value.
#'
#' @examples
#' 
#' FisherZ(0.8)
#' FisherZinv(FisherZ(-0.5))
#'
#' @rdname FisherZ
#' @export
FisherZ <- function (rho) 
{
    0.5 * log((1 + rho)/(1 - rho))
}

#' @rdname FisherZ
#' @export
FisherZinv <- function (z) 
{
  (exp(2 * z) - 1)/(exp(2 * z) + 1)
}
