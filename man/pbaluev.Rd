\name{pbaluev}
\alias{pbaluev}

\title{
False alarm probability
}
\description{
Computes the statistical significance of peaks (range 0-1) in the standardized periodogram. Typically not called by the user.
}
\usage{
pbaluev(Z,fmax,tm)
}

\arguments{
  \item{Z}{
the height of a periodogram peak
}
  \item{fmax}{
the highest frequency inspected
}
  \item{tm}{
a vector with measurement timepoints
}

}
\details{
Based on results in extreme value theory, improved analytic estimations of false alarm probabilities are given.
}
\value{
Returns the significance of the largest peak in the periodogram.
}
\references{
Baluev, R. V. (2008). Assessing the statistical significance of periodogram peaks.\emph{ Monthly Notices of the Royal Astronomical Society}, \bold{385(3)}, 1279-1285.
}
\author{
Thomas Ruf \email{thomas.ruf@vetmeduni.ac.at}.
}
\note{
Code based on astropy.timeseries
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{summary.lsp}}
}
\examples{
pbaluev(0.19,2.0,1:100)
}

\keyword{ ts }

