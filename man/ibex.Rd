\name{ibex}
\alias{ibex}
\docType{data}
\title{
Rumen Temperature In An Alpine Ibex
}
\description{
Telemetric measurements of rumen temperature in a free-living alpine ibex (\emph{Capra ibex}) measured at unequal time intervals. 
}
\usage{data(ibex)}
\format{
  A data frame with 1201 observations on 3 variables.
  \describe{
    \item{date}{a character variable giving date and time of measurements.}
    \item{hours}{a numerical variable giving hours elapsed since the first measurement.}
    \item{temp}{a numerical variable giving rumen (stomach) temperature in degrees Celsius.}
    
  }
}
\source{
A subset of data from Signer, C., Ruf, T., Arnold, W. (2011) \emph{Functional Ecology} \bold{25}: 537-547.
}
\examples{
data(ibex)
datetime <- as.POSIXlt(ibex$date)
plot(datetime,ibex$temp,pch=19,cex=0.3)
}
\keyword{datasets}
