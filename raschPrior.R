#' Prior probability of theta parameters in the Rasch model.
#'
#' Determine the prior probability of a proposed value of theta.
#'
#' @param theta A numeric vector of length one giving a proposed value of theta
#'
#' @return A numeric vector of length one giving the prior probability of the
#'   proposed value of theta
#' @author JB Duck-Mayr
#' @seealso \code{\link{Rasch-class}}, \code{\link{raschProbability}},
#'   \code{\link{raschLikelihood}}, \code{\link{raschEAP}}
#' @rdname raschPrior
#' @aliases raschPrior,ANY-method
#' @export
setGeneric(name='raschPrior',
           def=function(theta)
           {standardGeneric('raschPrior')}
)

#' @export
setMethod(f='raschPrior',
          definition=function(theta){
            return(dnorm(x=theta, mean=0, sd=3))
          }
)
