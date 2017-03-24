##
#' An object with a test respondent's name and responses, and item parameters.
#' 
#' Object of class \code{Rasch}
#' 
#' @slot name A character vector giving the name of the test taker
#' @slot a A numeric vector giving the question-item parameters
#' @slot y A numeric vector of answers for the respondent; a value of zero
#'   indicates the respondent did not answer correctly and a value of one
#'   indicates the respondent did answer correctly
#' 
#' @author JB Duck-Mayr
#' @aliases Rasch-class initialize,Rasch-method
#' @rdname Rasch
#' @export
setClass('Rasch',
         slots=c(name='character', a='numeric', y='numeric')
)

## Validity function:
#' @export
setValidity('Rasch',
            function(object){
              if (!all(object@y %in% c(0, 1))) {
                return('y slot should only contain 0 and 1 values')
              }
              if (length(object@a) != length(object@y)) {
                return('Every question must have a value for y and a')
              }
            }
)
