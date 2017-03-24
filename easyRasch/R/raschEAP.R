#' Expected a posteriori value of theta parameter for the Rasch model.
#'
#' Determine the expected a posteriori value of theta given the responses we
#' have observed from a test taker.
#' 
#' The expected a posteriori value (EAP) of theta is calculated by taking the
#' integral of \eqn{L(\theta_j|y_j)\pi(\theta_j)}, where \eqn{\pi(\theta_j)}
#' is the prior function for a proposed \eqn{\theta_j}. While the EAP is
#' defined as the integral from negative to positive infinity, this function
#' approximates the integral between specified upper and lower limits.
#'
#' @param raschObj An object of class Rasch
#' @param lower A numeric vector of length one giving the lower limits of
#'   integration; the default value is 6
#' @param upper A numeric vector of length one giving the upper limits of
#'   integration; the default value is 6
#'
#' @return A numeric vector of length one giving the expected a posteriori
#'   value of theta
#' @author JB Duck-Mayr
#' @seealso \code{\link{Rasch-class}} \code{\link{raschProbability}}
#' @rdname raschEAP
#' @aliases raschEAP,ANY-method
#' @export
setGeneric(name='raschEAP',
           def=function(raschObj, lower=-6, upper=6)
           {standardGeneric('raschEAP')}
)

#' @export
setMethod(f='raschEAP',
          definition=function(raschObj, lower=-6, upper=6){
            return(integrate(Vectorize(function(x) raschLikelihood(raschObj, x)
                             * raschPrior(x)), lower=lower, upper=upper)$value)
          }
)

