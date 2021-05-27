\name{lomb-package}
\alias{lomb-package}
\alias{lomb}
\docType{package}
\title{
Lomb-Scargle Periodogram
}
\description{
The Lomb-Scargle periodogram is the most widely used method to detect even weak periodic components in unequally sampled time series.  It can also be used for equally sampled time series.
}
\details{
\tabular{ll}{
Package: \tab lomb\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2021-02-01\cr
License: \tab GPL-3\cr
}
Function \code{\link{lsp}} computes the Lomb-Scargle periodogram for unevenly sampled times series (e.g., series with missing data).  P-values for the false-alarm probability of the highest peak in the periodogram are computed.  Alternatively, function \code{\link{randlsp}} computes a bootstrap P-value for the largest peak in the periodogram by repeatedly randomizing the time-series sequence.  Both functions allow setting the range of frequencies to be inspected, as well as the stepsize (oversampling factor) used for frequency scanning.
}
\author{
Thomas Ruf
\cr
Department of Interdisciplinary Life Sciences,
\cr
University of Veterinary Medicine, Vienna, Austria
\cr
Maintainer: Thomas Ruf \email{thomas.ruf@vetmeduni.ac.at}
}

\references{
Ruf, T. (1999) The Lomb-Scargle Periodogram in Biological Rhythm Research: Analysis of Incomplete and Unequally Spaced Time-Series. \emph{Biological Rhythm Research} \bold{30}: 178--201
}
\keyword{ package }

\examples{
data(lynx)
lsp(lynx)
}
