\name{actogram}
\alias{actogram}

\title{
Plot actogram
}
\description{
plots an actogram for a time series with irregular (or regular) sampling intervals.  
}
\usage{
actogram(date, response,from ,to, scalefac, subtract, dble,
  dig, border, fill, grad, lwd, photo,latitude, longitude, zone, twilight)
}


\arguments{
  \item{date}{data datetime, format as in as.Date}
  \item{response}{which variable to plot}
  \item{from}{date for start of subsection}
  \item{to}{date for end of subsection}
  \item{scalefac}{one day is 1.0 wide. Use >1 if plots are exaggerated }
  \item{subtract}{value to subtract from response.}
  \item{dble}{logical Double plot? Ie day1 day2, day2 day3, day3 day4 ...}
  \item{dig}{logical Digitize to 0 1 based on mean?}
  \item{border}{border_colour of rectangle edges }
  \item{fill}{colour of rectangle fills, relevant only if timepoint separation is large}
  \item{grad}{logical Plot gradient?}
  \item{lwd}{line width of rectangles}
  \item{photo}{logical Plot photoperiod? Photo is true when location is entered.}
  \item{latitude}{animal location}
  \item{longitude}{animal location}
  \item{zone}{time zone of location. Default:0 = Greenwich}
  \item{twilight}{"rise/set", "civil" or "nautic"}  
}

\details{
This function plots actograms of both unevenly and evenly sampled data. It only requires data in standard R format, no special rhythms format is necessary.
}

\examples{
\dontrun{

data(caradat)
actogram(caradat$Date, caradat$Activity, dble=TRUE, photo=FALSE, dig=TRUE, fill="blue")


data(deerdat)
actogram(deerdat$Zeit, deerdat$Akt, grad=TRUE,from="2010-10-01 00:00:00",to="2011-03-31 00:00:00",
latitude=47.1415,longitude=9.5215,zone=1,twilight="nautic")
}
}