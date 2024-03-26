\name{plot.lsp}
\alias{plot.lsp}
\title{
Plot Lomb-Scargle Periodogram
}
\description{
Plots the normalised power as a function of frequency (or period, depending on type in function lsp).
}
\usage{
\method{plot}{lsp}(x, main = "Lomb-Scargle Periodogram", xlabel = NULL,
    ylabel = "normalized power", level = TRUE, plot=TRUE, ...)
}

\arguments{
  \item{x}{
Object of class lsp as returned from function lsp.
}

  \item{main}{
Character. Main title of the periodogram plot. Defaults to \dQuote{Lomb-Sargle Periodogram}.
}
  \item{xlabel}{
Character. X-axis label of the periodogram plot.
}
  \item{ylabel}{
Character. Y-axis label of the periodogram plot. 
}
  \item{level}{
Logical. If TRUE,  the significance level is displayed as a dashed line.
}
  \item{plot}{
If TRUE, the periodogram is plotted.
}
  \item{\dots}{
Additional graphics parameters
}
}
\details{
Usually, this function is only called by function lsp. It maybe called by the user for some control of the output.  For better control, plot results from lsp ($scanned, $power) as desired.
}
\value{
Invisibly returns the object of class lsp it is called with.
}
\author{
Thomas Ruf \email{thomas.p.ruf@me.com}
}

\seealso{
 \code{\link{lsp}}
}
\examples{
data(ibex)
ibex.spec <- lsp(ibex[,2:3],type='period', from=12,to=36,ofac=10, plot=FALSE)

plot.lsp(ibex.spec, main="Tb in Capra ibex",xlabel="Period (h)",ylabel="Power",level=FALSE)

}

\keyword{ts}

