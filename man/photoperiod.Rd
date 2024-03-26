\name{photoperiod}
\alias{photoperiod}
\title{
Computes photoperiod
}
\description{
computes sunrise & sunset for day of year and location
}
\usage{
photoperiod(dayofyear, latitude, longitude, zone = 0, twilight = "civil")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dayofyear}{
day of ywar 1-366
}
  \item{latitude}{
e.g. 42.00
}
 \item{longitude}{
e.g. 9.00
}
  \item{zone}{
time zone e.g 1 (Vienna)
}
  \item{twilight}{
"rise/set", "civil" or "nautic"
}
}

\value{
\item{sunrise}{vector of sunrises}
\item{set}{vector of sunsets }
}

\references{
http://lexikon.astronomie.info/zeitgleichung/
}


\examples{
photoperiod (180, 42,9, zone=1, twilight="civil")
}

