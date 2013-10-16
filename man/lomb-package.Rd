\name{lomb-package}
\alias{lomb-package}
\alias{lomb}
\docType{package}
\title{
Lomb-Scargle Periodogram
}
\description{
The Lomb-Scargle periodogram is the most wideley used method to detect even weak periodoc components in unequally sampled time series.  It can also be used for equally sampled time series.
}
\details{
\tabular{ll}{
Package: \tab lomb\cr
Type: \tab Package\cr
Version: \tab 1.0-0\cr
Date: \tab 2013-10-16\cr
License: \tab GPL-2\cr
}
Function \code{\link{lsp}} computes the Lomb-Scargle periodogram for unevenly sampled times series (e.g., series with missing data).  P-values for the highest peak in the periodogram are computed from the exponential distribution.  Alternatively, function \code{\link{randlsp}} computes a p-value for the largest peak in the periodogram by repeatedly randomising the time-series sequence.  Both functions allow setting the range of frequencies to be inspected, as well as the stepsize (oversampling factor) used for frequency scanning.
}
\author{
Thomas Ruf
\cr
Department of Integrative Biology and Evolution,
\cr
University of Veterinary Medicine, Vienna, Austria
\cr
Maintainer: Thomas Ruf \email{thomas.ruf@vetmeduni.ac.at}
}

\references{
Ruf, T. (1999) The Lomb-Scargle Periodogram in Biological Rhythm Research: Analysis of Incomplete and Unequally Spaced Time-Series. \emph{Biological Rhythm Research} \bold{30}: 178--201
}
\keyword{ package }
\seealso{
Other versions of the Lomb-Scargle periodogram are implemented in packages cts
(\code{\link[cts]{spec.ls}}) and nlts (\code{\link[nlts]{spec.lomb}})
}
\examples{
data(lynx)
lsp(lynx)
}
