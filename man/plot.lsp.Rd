\name{plot.lsp}
\alias{plot.lsp}
\title{
Plot Lomb-Scargle Periodogram
}
\description{
Plots the normalised power as a function of frequency (or period, depending on type in function lsp).
}
\usage{
\method{plot}{lsp}(x, type = "l", main = "Lomb-Scargle Periodogram", xlab = NULL,
    ylab = "normalised power", level = TRUE, log = NULL, ...)
}

\arguments{
  \item{x}{
Object of class lsp as returned from function lsp.
}
  \item{type}{
Character indicating the type of plotting.  Any of the types as in plot.default.
}
  \item{main}{
Character. Main title of the periodogram plot. Defaults to \dQuote{Lomb-Sargle Periodogram}.
}
  \item{xlab}{
Character. X-axis label of the periodogram plot.
}
  \item{ylab}{
Character. Y-axis label of the periodogram plot. 
}
  \item{level}{
Logical. If TRUE,  the siginificance level is displayed as a dashed line.
}
  \item{log}{
By default,  periodgrams of type=\dQuote{period} are shown with a log x-axis.  If desired otherwise, use log=\dQuote{..} to define log-axis as in plot.default
}
  \item{\dots}{
Additional graphics parameters
}
}
\details{
Usually, this function is only called by function lsp. It maybe called by the user for some control of the ouput.  For better control, plot results from lsp ($scanned, $power) as desired.
}
\value{
Invisibly returns the object of class lsp it is called with.
}
\author{
Thomas Ruf \email{thomas.ruf@vetmeduni.ac.at}
}

\seealso{
 \code{\link{lsp}}
}
\examples{
data(ibex)
ibex.spec <- lsp(ibex[,2:3],type='period',from=12,to=36,ofac=10, plot=FALSE)
op <- par(pch=16)
plot.lsp(ibex.spec, main="Periodogram of daily rhythms of Tb in Capra ibex",
    cex.lab=1.3,log="", type="b",level=FALSE,xaxt="n")
axis(side=1,at=seq(12,36,by=6))
par(op)
}

\keyword{ts}

