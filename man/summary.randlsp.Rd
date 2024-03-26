\name{summary.randlsp}
\alias{summary.randlsp}

\title{
Summarise Randomised Lomb-Scargle Periodogram Results 
}
\description{
Summary method for class randlsp.
}
\usage{
\method{summary}{randlsp}(object,...)
}

\arguments{
  \item{object}{
an object of class randlsp.
}
  \item{...}{
currently, no other arguments are required.
}
}

\value{
summary.randlsp returns a one column data.frame with results from function randlsp.  Row names and contents are as follows:
\item{Time}{Name of the sampling time variable.}
\item{Data}{Name of the measured variable.}
\item{Type}{either \dQuote{frequency} or \dQuote{period}.}
\item{Oversampling}{The degree of oversampling (>=1).}
\item{From}{The lowest frequency (or period, depending on type) inspected.}
\item{To}{The highest frequency (or period, depending on type) inspected.}
\item{# frequencies}{The number of frequencies (or periods, depending on type) inspected.}
\item{PNmax}{The peak normalised power in the periodogram.}
\item{At frequency}{The frequency at which PNmax occurred.}
\item{At period}{The period at which PNmax occurred.}
\item{Repeats}{The number of randomisations.}
\item{P-value (PNmax)}{The probability that PNmax occurred by chance, computed from randomising the data sequence.}
}

\author{
Thomas Ruf \email{thomas.p.ruf@me.com}
}
\seealso{
\code{\link{randlsp}}
}
\examples{
data(lynx)
summary(randlsp(repeats=500,lynx))
}
\keyword{ ts }
