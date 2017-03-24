data('easyRaschExample')
## Examples of plot method using one plot panel:
invisible(sapply(easyRaschExample, plot))
## Examples of plot method using lattice graphics:
invisible(sapply(easyRaschExample, function(x) print(plot(x, together=FALSE))))
## Expected a posteriori value of theta for Mary:
(MaryEAP <- raschEAP(easyRaschExample$Mary))
## How much do we learn about Mary from each question on this test?
raschInformation(easyRaschExample$Mary, MaryEAP)
## What do we learn about test takers of all abilities?
raschTestInformationCurve(easyRaschExample$Mary)