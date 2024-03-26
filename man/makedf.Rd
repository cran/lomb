\name{makedf}
\alias{makedf}

\title{
makes a data.frame from an actogram for lsp
}
\description{
Converts an actogram to a periodogram
}
\usage{
makedf(tvar, pvar)
}

\arguments{
  \item{tvar}{data datetime}
  \item{pvar}{which variable to plot}
}

\value{
a data.frame with two colums: time and variable (eg. activity)
}


\examples{
 \dontrun{
 
data(caradat) #unevenly sampled
focus=actogram(caradat$Date, caradat$Activity, dble=TRUE, photo=FALSE, zone=1, 
  from="1970-01-01 00:00:00",to="1970-01-14 00:00:00")
df=makedf (focus$date, focus$plotvar)
lsp(df, type="period",ofac=5,from=12,to=36)


data(layla) #evenly sampled
focus=actogram(layla$Date,layla$Activity,latitude=48.20, longitude=16.37,  zone=2, dig=TRUE)
df=makedf (focus$date, focus$plotvar)
lsp(df, type="period",ofac=5,from=20,to=50)
 }
} 

