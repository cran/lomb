\name{summary.lsp}
\alias{summary.lsp}

\title{
Summarise Lomb-Scargle Periodogram Results 
}
\description{
Summary method for class lsp.
}
\usage{
\method{summary}{lsp}(object,...)
}
\arguments{
  \item{object}{
an object of class lsp.
}
  \item{...}{
currently, no other arguments are required.
}
}
\value{
summary.lsp returns a one column data.frame with results from function lsp.  Row names and contents are as follows:
\item{Time}{Name of the sampling time variable.}
\item{Data}{Name of the measured variable.}
\item{Type}{either \dQuote{frequency} or \dQuote{period}.}
\item{Oversampling factor}{The degree of oversampling (>=1).}
\item{From}{The lowest frequency (or period, depending on type) inspected.}
\item{To}{The highest frequency (or period, depending on type) inspected.}
\item{# frequencies}{The number of frequencies (or periods, depending on type) inspected.}
\item{PNmax}{The peak normalised power in the periodogram.}
\item{At frequency}{The frequency at which PNmax occurred.}
\item{At period}{The period at which PNmax occurred.}
\item{P-value (PNmax)}{The probability that PNmax occurred by chance, computed from the exponential distribution.}
}

\author{
Thomas Ruf \email{thomas.p.ruf@me.com}
}

\seealso{
\code{\link{lsp}}
}
\examples{
data(lynx)
summary(lsp(lynx))
}

\keyword{ ts }

