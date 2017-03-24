#' Likelihood function for the Rasch model.
#'
#' Determine the likelihood of a proposed value of theta given the responses
#' we have observed from a test taker.
#' 
#' The likelihood of a proposed value of theta given the responses we have
#' observed from a test taker is given by the product of the probability of
#' each observed response given the item parameters and the proposed value of
#' theta. Because it is more numerically stable, this function uses the
#' exponentiation of the sum of the logged probabilities.
#'
#' @param raschObj An object of class Rasch
#' @param theta A numeric vector of length one giving a proposed value of theta
#'
#' @return A numeric vector of length one giving the likelihood given y & theta
#' @author JB Duck-Mayr
#' @seealso \code{\link{Rasch-class}} \code{\link{raschProbability}}
#' @rdname raschLikelihood
#' @aliases raschLikelihood,ANY-method
#' @export
setGeneric(name='raschLikelihood',
           def=function(raschObj, theta)
           {standardGeneric('raschLikelihood')}
)

#' @export
setMethod(f='raschLikelihood',
          definition=function(raschObj, theta){
            probabilities <- raschProbability(raschObj, theta)
            return(exp(sum(log(probabilities[['answerProbabilities']]))))
          }
)
