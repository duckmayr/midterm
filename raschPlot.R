#' Printing a Rasch object
#'
#' For an object of class \code{Rasch}, prints the item characteristic curves.
#' The x axis is possible values of \eqn{\theta_j}; the y axis is \eqn{P_ij}.
#'
#' @param raschObj An object of class Rasch
#' @param EAP Should a dashed line appear showing the EAP for \eqn{\theta_j}?
#' @param together Should the item characteristic curves be drawn in one
#'   plotting panel, or should they be drawn in separate panels using lattice
#'   graphics?
#'
#' @author JB Duck-Mayr
#' @seealso \code{\link{Rasch-class}}
#' @export
setMethod(f='plot', signature='Rasch',
          definition=function(x, EAP=TRUE, together=TRUE){
            xAxis <- seq(from=-3, to=3, by=0.1)
            probs <- sapply(xAxis, function(i){
              raschProbability(x, i)$questionProbabilities})
            if (together) {
              plot(NULL, xlim=c(-3, 3), ylim=c(0, 1), xlab=expression(theta),
                   ylab='Probability of Correct Answer',
                   main=x@name)
              apply(probs, 1, function(y) lines(x=xAxis, y=y))
              if (EAP) {
                abline(v=raschEAP(x), lty=2)
              }
            } else {
              plotData <- data.frame(prob=as.vector(t(probs)),
                                     theta=rep(xAxis, nrow(probs)),
                                     question=rep(1:nrow(probs), each=61))
              sLabs <- paste0('Q', 1:nrow(probs))
              lattice::xyplot(prob ~ theta|factor(question), plotData,
                              type='l', xlab=expression(theta), main=x@name,
                              ylab='Probability of Correct Answer',
                              strip=lattice::strip.custom(factor.levels=sLabs))
            }
          }
)
