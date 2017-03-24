#' Printing a Rasch object
#'
#' For an object of class \code{Rasch}, prints the item characteristic curves.
#' The x axis is possible values of \eqn{\theta_j}; the y axis is \eqn{P_ij}.
#'
#' @param x An object of class Rasch
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
            mainText <- paste('Item Characteristic Curves for', x@name)
            xAxis <- seq(from=-3, to=3, by=0.1)
            probs <- sapply(xAxis, function(i){
              raschProbability(x, i)$questionProbabilities})
            if (together) {
              plot(NULL, xlim=c(-3, 3), ylim=c(0, 1), xlab=expression(theta),
                   ylab='Probability of Correct Answer',
                   main=mainText)
              sapply(1:nrow(probs), function(y){
                lines(x=xAxis, y=probs[y, ], col=y)
              })
              legendText <- paste0('Q', 1:nrow(probs))
              legendCols <- palette()[1:nrow(probs)]
              legendLty <- 1
              # apply(probs, 1, function(y) lines(x=xAxis, y=y, col=y))
              if (EAP) {
                abline(v=raschEAP(x), lty=2)
                legendText <- c(legendText, 'EAP')
                legendCols <- c(legendCols, 'black')
                legendLty <- c(rep(1, nrow(probs)), 2)
              }
              legend('topleft', legend=legendText, bty='n', col=legendCols,
                     lty=legendLty)
            } else {
              plotData <- data.frame(prob=as.vector(t(probs)),
                                     theta=rep(xAxis, nrow(probs)),
                                     question=rep(1:nrow(probs), each=61))
              sLabs <- paste0('Q', 1:nrow(probs))
              if (EAP){
                xyplot(prob ~ theta|factor(question), plotData, type='l',
                       xlab=expression(theta), main=mainText,
                       ylab='Probability of Correct Answer',
                       strip=strip.custom(factor.levels=sLabs),
                       panel= function(...){
                         panel.abline(v=raschEAP(x), lty='dashed')
                         panel.xyplot(...)
                       }
                )
              } else {
                xyplot(prob ~ theta|factor(question), plotData, type='l',
                       xlab=expression(theta), main=mainText,
                       ylab='Probability of Correct Answer',
                       strip=strip.custom(factor.levels=sLabs))
              }
            }
          }
)
