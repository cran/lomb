\name{layla}
\alias{layla}
\docType{data}
\title{
Activiy of a dog.
}
\description{
activity and body temperature of a domestic dog in summer 
}
\usage{data("layla")}

\format{
  A data frame with 10120 observations on the following 4 variables.
  \describe{
    \item{Id}{a vector with animal ID} 
    \item{Date}{a vector with date and time of day}
    \item{Temperature}{a numeric vector of body temperature}
    \item{Activity}{a numeric vector of kocomtor activity}
  }
}
\examples{
data(layla)
## maybe str(layla) ; plot(layla) ...
}
\keyword{datasets}
