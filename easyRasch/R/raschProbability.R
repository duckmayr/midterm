#' Probability function for the Rasch model.
#'
#' Determine the probabilities a respondent answered a question correctly and
#' that they gave the observed answer for a proposed theta value.
#' 
#' The probability a respondent answers a question correctly depends both on
#' the difficulty of the question and the ability of the respondent. For some
#' proposed value of theta, a parameter for the respondent's ability, this
#' function returns a list of two numeric vectors, each of length n (the number
#' of questions); the first vector gives Pij, or the probability the respondent
#' answered each question j correctly, while the second gives the probability
#' the respondent answered the way they we observed them responding (i.e. Pij
#' if we observe a correct answer and Qij = 1 - Pij if we observe an incorrect
#' answer).
#'
#' @param raschObj An object of class Rasch
#' @param theta A numeric vector of length one giving a proposed value of theta
#'
#' @return A list with the following elements:
#'   \item{questionProbabilities}{A numeric vector of length n that represents
#'   the probability of a respondent with the proposed theta paramter answering
#'   correctly for each question}
#'   \item{answerProbabilities}{A numeric vector of length n that represents
#'   the probability the respondent \code{rashObj} answered they way they did}
#' @author JB Duck-Mayr
#' @seealso \code{\link{Rasch-class}}
#' @rdname raschProbability
#' @aliases raschProbability,ANY-method
#' @export
setGeneric(name='raschProbability',
           def=function(raschObj, theta)
           {standardGeneric('raschProbability')}
)

#' @export
setMethod(f='raschProbability',
          definition=function(raschObj, theta){
            commonExpression <- exp(theta - raschObj@a) # used in num and denom
            questionProbabilities <- commonExpression/(1 + commonExpression)
            return(list(questionProbabilities=questionProbabilities,
                        answerProbabilities=ifelse(raschObj@y == 1,
                                                   questionProbabilities,
                                                   1 - questionProbabilities)))
          }
)
