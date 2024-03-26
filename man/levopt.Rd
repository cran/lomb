\name{levopt}
\alias{levopt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{compute level
%%  ~~function to do ... ~~
}
\description{
utility function to determine deviation from p-value
}
\usage{
levopt(x, alpha, fmax, tm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  vector with start values
}
  \item{alpha}{
  desired significance level
}
  \item{fmax}{  
  the highest frequency inspected
}
  \item{tm}{
  a vector with measurement timepoints
}
}
\value{
  (log(prob)-log(alpha))^2
}

\author{
Thomas Ruf \email{thomas.p.ruf@me.com}
}

 \keyword{ ts }
