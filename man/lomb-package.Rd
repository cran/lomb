\name{lomb-package}
\alias{lomb-package}
\alias{lomb}
\docType{package}
\title{
Lomb-Scargle Periodogram
}
\description{
The Lomb-Scargle periodogram is the most widely used method to detect even weak periodic components in unequally sampled time series.  It can also be used for equally sampled time series.The oacka alao plots actograms and converts them to periodograms.
}
\details{
\tabular{ll}{
Package: \tab lomb\cr
Type: \tab Package\cr
Version: \tab 2.1.0\cr
Date: \tab 2022-02-22\cr
License: \tab GPL-3\cr
}
Function \code{\link{lsp}} computes the Lomb-Scargle periodogram for unevenly sampled times series (e.g., series with missing data).  P-values for the highest peak in the periodogram are computed from the exponential distribution.  Alternatively, function \code{\link{randlsp}} computes a p-value for the largest peak in the periodogram by repeatedly randomising the time-series sequence.  Both functions allow setting the range of frequencies to be inspected, as well as the stepsize (oversampling factor) used for frequency scanning. Function \code{\link{actogram}} plots actograms and function \code{\link{makedf}} prepares them for lsp.
}
\author{
Thomas Ruf
\cr
Department of Interdisciplinary Life Sciences,
\cr
University of Veterinary Medicine, Vienna, Austria
\cr
Maintainer: Thomas Ruf \email{thomas.p.ruf@me.com}
}

\references{
Ruf, T. (1999) The Lomb-Scargle Periodogram in Biological Rhythm Research: Analysis of Incomplete and Unequally Spaced Time-Series. \emph{Biological Rhythm Research} \bold{30}: 178--201
}
\keyword{ package }

\examples{
data(lynx)
lsp(lynx)
}
