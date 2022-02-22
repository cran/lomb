\name{pershow}
\alias{pershow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{show periodogram
%%  ~~function to do ... ~~
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

}
}

\author{
Thomas Ruf \email{thomas.ruf@vetmeduni.ac.at}
}

\seealso{
\code{\link{getpeaks}}
}
\examples{

per=lsp(lynx,ofac=5)
pershow(per)
}

 \keyword{ ts }

