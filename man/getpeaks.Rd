\name{getpeaks}
\alias{getpeaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve periodogram peaks
%%  ~~function to do ... ~~
}
\description{
Retrieves and displays the npeaks largest peaks in the periodogram-
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getpeaks(object,npeaks,plotit)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{  object must be of class "lsp"}
  \item{npeaks}{  number of peaks to get} 
  \item{plotit}{  if TRUE show plot}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
Returns a list with
\item{data}{A dataframe with times an heights of peaks}
\item{plot}{An annotated periodogram}
%% ...
}

\author{
Thomas Ruf \email{thomas.p.ruf@me.com}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{show}}
}
\examples{

per=lsp(lynx,ofac=5)
getpeaks(per,6) # obtain the 6 largest  peaks
}

\keyword{ ts }

