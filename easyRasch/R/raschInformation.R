#' Fisher Information from questions in a Rasch model for a given test ability.
#'
#' Determine the Fisher Information for each question in a test for a test
#' taker with a proposed theta value.
#'
#' @param raschObj An object of class \code{Rasch}
#' @param theta A numeric vector of length one giving a proposed value for
#'   theta
#'
#' @return A numeric vector of length n (the number of test questions) giving
#'   Fisher's information for each question for a test taker of ability theta
#' @author JB Duck-Mayr
#' @seealso \code{\link{Rasch-class}}
#' @rdname raschInformation
#' @aliases raschInformation,ANY-method
#' @export
setGeneric(name='raschInformation',
           def=function(raschObj, theta)
           {standardGeneric('raschInformation')}
)

#' @export
setMethod(f='raschInformation',
          definition=function(raschObj, theta){
            Pij <- raschProbability(raschObj, theta)$questionProbabilities
            return(Pij * (1 - Pij))
          }
)

