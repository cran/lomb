\name{levopt}
\alias{levopt}

\title{
Internal function to find significance level
}
\description{
Called by optim to find significance value (0-1) corresponding to p-value (x).
}
\usage{
levopt(x, alpha, fmax, tm)
}

\arguments{
  \item{x}{
Initial value of parameter to be optimized.
}
  \item{alpha}{
Desired significance level, defaults to 0.01.
}
  \item{fmax}{
Maximum frequency inspected.
}
  \item{tm}{
Vector of times inspected.
}
}

\value{
Scalar. The value at which the periodogram reaches significance.
}
\references{
VanderPlas, J. & Ivezic, Z. (2015) Periodograms for Multiband Astronomical Time Series.\emph{The Astrophysical Journal} \bold{812.1}:18
}
\author{
Thomas Ruf \email{thomas.ruf@vetmeduni.ac.at}.
}




\seealso{
pbaluev
}
\examples{
##---- Do not call directly, internal function
}
