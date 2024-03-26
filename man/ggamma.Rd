\name{ggamma}
\alias{ggamma}

\title{
Utility function called by pbaluev()
}
\description{
From astropy.timeseries
}
\usage{
ggamma(N)
}

\arguments{
  \item{N}{
A positive number}
}

\value{
sqrt(2 / N) * exp(lgamma(N / 2) - lgamma((N - 1) / 2))
}
\references{
VanderPlas, J. & Ivezic, Z. (2015) Periodograms for Multiband Astronomical Time Series.\emph{The Astrophysical Journal} \bold{812.1}:18
}
\author{
Thomas Ruf \email{thomas.p.ruf@me.com}
}


\seealso{
\code{\link{pbaluev}}
}
\examples{
ggamma(3)
}


\keyword{ ts }