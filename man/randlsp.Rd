\name{randlsp}
\alias{randlsp}

\title{
Randomise Lomb-Scargle Periodogram
}
\description{
randlsp is used to obtain robust p-values for the significance of the largest peak in a Lomb-Scargle periodogram by randomisation.  The data sequence is scrambled repeatedly and the probability of random peaks reaching or exceeding the peak in the original (unscrambled) periodogram is computed.
}
\usage{
randlsp(repeats=1000,x, times = NULL, from = NULL, to = NULL,
    type = c("frequency", "period"), ofac = 1, alpha = 0.01,
    plot = TRUE, trace = TRUE, ...)
}
\arguments{
  \item{repeats}{
An integer determining the number of repeated randomisations.  Large numbers (>=1000) are better but can make the procedure time-consuming.
}
  \item{x}{
The data to be analysed.  x can be either a two-column numerical dataframe or matrix, with sampling times in columnn 1 and measurements in column 2, a single numerical vector containing measurements, or a single vector \code{\link{ts}} object (which will be converted to a numerical vector). }

  \item{times}{
If x is a single vector, times can be provided as a numerical vector of equal length containing sampling times.  If x is a vector and times is NULL, the data are assumed to be equally sampled and times is set to 1:length(x).
}
  \item{from}{
The starting frequency (or period, depending on type) to begin scanning for periodic components.
}
  \item{to}{
The highest frequency (or period, depending on type) to scan.
}
  \item{type}{
Either \dQuote{frequency} (the default) or \dQuote{period}.  Determines the type of the periodogram x-axis.
}
  \item{ofac}{
The oversampling factor.  Must be an integer >=1.  Larger values of ofac lead to finer scanning of frequencies but may be time-consuming for large datasets and/or large frequency ranges (from...to).
}
  \item{alpha}{
The significance level. The periodogram plot shows a horizontal dashed line.  Periodogram peaks exceeding this line can be considered significant at alpha.   Defaults to 0.01.  Only used if plot=TRUE.
}
  \item{plot}{
Logical.  If TRUE, two plots are displayed (i)  The periodogram of the original (unscrambled) data (ii)  A histogram of peaks occurring by chance during sequence randomisation.  A vertical line is drawn at the height of the peak in a periodogram of the original data. 
}
  \item{trace}{
Logical.  If TRUE, information about the progress of the randomisation procedure is printed during the running of randlsp. 
}
  \item{\dots}{
Additional graphical parameters affecting the histogram plot.
}
}
\details{
Function randlsp preserves the actual measurement intervals, which may affect the periodogram (see Nemec & Nemec 1985, below).  Hence, this is a conservative randomisation procedure. 

P-values from both randlsp and \code{\link{lsp}} increase with the number of frequencies inspected.  Therefore, if the frequency-range of interest can be narrowed down \emph{a priori}, use arguments \dQuote{from} and \dQuote{to} to do so.
}
\value{
A named list with the following items:
\item{scanned}{A vector containing the frequencies/periods scanned.}
\item{power}{A vector containing the normalised power corresponding to scanned frequencies/periods.}
\item{data}{Names of the data vectors analysed.}
\item{n}{The length of the data vector.}
\item{type}{The periodogram type used, either \dQuote{frequency} or \dQuote{period}.}
\item{ofac}{The oversampling factor used.}
\item{n.out}{The length of the output (powers). This can be >n if ofac >1.}
\item{peak}{The maximum power in the frequency/period interval inspected.}
\item{peak.at}{The frequency/period at which the maximum peak occurred.}
\item{random.peaks}{A vector of peaks (with length=repeats) of maximum power values computed from randomised data.}
\item{repeats}{The number of randomisations.}
\item{p.value}{The probability that the peak in the original data occurred by chance, computed from randomising the data sequence.}
}
\references{
Nemec A.F.L, Nemec J.M. (1985) A test of significance for periods derived using phase-dispersion-miminimization techniques. \emph{The Astronomical Journal} \bold{90}:2317--2320
}
\author{
Thomas Ruf \email{thomas.p.ruf@me.com}
}
\seealso{
\code{\link{lsp}}
}
\examples{
data(lynx)
set.seed(444)
rand.times <- sample(1:length(lynx),30) # select a random vector of sampling times
randlsp(repeats=1000,lynx[rand.times],times=rand.times)
}
\keyword{ ts }

