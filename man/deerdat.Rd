\name{deerdat}
\alias{deerdat}
\docType{data}
\title{
Locomotor activiy of a deer.
}
\description{
Locomotor activiy and other variables of a red deer free-living in the alps.
}
\usage{data("deerdat")}
\format{
  A data frame with 293826 observations on the following 9 variables.
  \describe{
    \item{\code{id}}{animal ID}
    \item{\code{Tiernummer}}{again}
    \item{\code{Halsbandnummer}}{collar number}
    \item{\code{Zeit}}{time}
    \item{\code{RepeaterTemp}}{gevice temperature}
    \item{\code{Bodytemp}}{body temperature}
    \item{\code{Kopfwechsel}}{number of head down movements}
    \item{\code{KopfuntenzeitSek}}{time head down}
    \item{\code{Akt}}{activity}
  }
}
\examples{
data(deerdat)
## maybe str(deerdat) ; plot(deerdat) ...
}
\keyword{datasets}
