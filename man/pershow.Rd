\name{pershow}
\alias{pershow}

\title{show periodogram
}
\description{
Shows a periodogram in browser window as line and dot plot.
When moving the cursor close to dots times an peak-heights of the periodogram are shown.

}
\usage{
pershow(object) # object of class "lsp"
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
  an object of class "lsp"
}
}

\author{
Thomas Ruf \email{thomas.p.ruf@me.com}
}

\seealso{
\code{\link{getpeaks}}
}
\examples{

per=lsp(lynx,ofac=2)
pershow(per)

#In Rstudio go to the viewer pane. Move mouse to point of interest.

}

 \keyword{ ts }

