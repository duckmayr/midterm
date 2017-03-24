#' Plot a Test Information Curve
#'
#' Plot the sum of Fisher Information for all test questions at a range of
#' theta values.
#'
#' @param raschObj An object of class \code{Rasch}
#' @param lower A numeric vector of length one giving the lowest theta value
#' @param lower A numeric vector of length one giving the highest theta value
#'
#' @author JB Duck-Mayr
#' @seealso \code{\link{Rasch-class}}
#' @rdname raschTestInformationCurve
#' @aliases raschTestInformationCurve,ANY-method
#' @export
setGeneric(name='raschTestInformationCurve',
           def=function(raschObj, lower=-3, upper=3)
           {standardGeneric('raschTestInformationCurve')}
)

#' @export
setMethod(f='raschTestInformationCurve',
          definition=function(raschObj, lower=-3, upper=3){
            xVals <- seq(from=lower, to=upper, by=0.1)
            yVals <- sapply(xVals, function(x){
              sum(raschInformation(raschObj, x))
              })
            plot(x=xVals, y=yVals, type='l',
                 xlab=expression(theta), ylab='Test Information',
                 main=paste('Test Information Curve for', raschObj@name))
          }
)

