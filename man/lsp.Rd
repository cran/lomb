\name{lsp}
\alias{lsp}
\title{
Lomb-Scargle Periodogram
}
\description{
Computes the Lomb-Scargle periodogram for a time series with irregular (or regular) sampling intervals.  Allows selecting a frequency range to be inspected, as well as the spacing of frequencies scanned.
}
\usage{
lsp(x, times = NULL, from = NULL, to = NULL, type = c("frequency", "period"),
    ofac = 1, alpha = 0.01, normalize=c("standard","press"), plot = TRUE, ...)
}
\arguments{
  \item{x}{
The data to be analysed.  x can be either a two-column numerical dataframe or matrix, with sampling times in column 1 and measurements in column 2, a single numerical vector containing measurements, or a single vector \code{\link{ts}} object (which will be converted to a numerical vector).
}
  \item{times}{
If x is a single vector, times can be provided as a numerical vector of equal length containing sampling times. If x is a vector and times is NULL, the data are assumed to be equally sampled and times is set to 1:length(x).
}
  \item{from}{
The starting frequency (or period, depending on type) to begin scanning for periodic components.
}
  \item{to}{
The highest frequency (or period, depending on type) to scan.
}
  \item{type}{
Either \dQuote{frequency} (the default) or \dQuote{period}. Determines the type of the periodogram x-axis.
}
  \item{ofac}{
The oversampling factor. Must be an integer>=1. Larger values of ofac lead to finer scanning of frequencies but may be time-consuming for large datasets and/or large frequency ranges (from...to).
}
  \item{alpha}{
The significance level. The periodogram plot shows a horizontal dashed line. Periodogram peaks exceeding this line can be considered significant at alpha. Defaults to 0.01. Only used if plot=TRUE.
}
  \item{normalize}{
The type of normalization used, either \dQuote{standard} or \dQuote{press}. If normalization is standard (the default) the periodogram is confined to the interval 0-1, and the statistical significance of the largest peak in the periodogram is computed according to Baluev (2008).if normalization is set to \dQuote{press} the periodogram will be normalized using the factor  1/(2 * var(y)) and the p-value for the significance of the largest peak in the periodogram is computed from the exponential distribution, as outlined in Press et al. (1994), see below
}
  \item{plot}{
Logical. If plot=TRUE the periodogram is plotted.
}
  \item{\dots}{
Further graphical parameters affecting the periodogram plot. 
}
}
\details{
For a more robust - but potentially time-consuming estimation of p-values (when n is large) see \code{\link{randlsp}}.

Significance levels in both lsp and \code{\link{randlsp}}  increase with the number of frequencies inspected.  Therefore, if the frequency-range of interest can be narrowed down \emph{a priori}, use arguments \dQuote{from} and \dQuote{to} to do so.
}
\value{
A named list with the following components:
\item{normalize}{The type of normalization used.}
\item{scanned}{A vector containing the frequencies/periods scanned.}
\item{power}{A vector containing the normalised power corresponding to scanned frequencies/periods.}
\item{data}{Names of the data vectors analysed.}
\item{n}{The length of the data vector.}
\item{type}{The periodogram type used, either "frequency" or "period".}
\item{ofac}{The oversampling factor used.}
\item{n.out}{The length of the output (powers). This can be >n if ofac >1.}
\item{alpha}{The false alarm probability used.}
\item{sig.level}{Powers > sig.level can be considered significant peaks at p=alpha.}
\item{peak}{The maximum power in the frequency/period interval inspected.}
\item{peak.at}{The frequency/period at which the maximum peak occurred.}
\item{p.value}{The probability that the maximum peak occurred by chance.}

}
\note{
For a description of the properties of the Lomb-Scargle Periodogram, its computation and comparison with other methods see Ruf, T. (1999).  Function lsp uses the algorithm given by Press et al (1994).  The Lomb-Scargle Periodogram was originally proposed by Lomb N.R. (1976) and further extended by Scargle J.D. (1982). An improved method for assessing the statistical significance of candidate periodicities by Baluev (2008), based on extreme value theory, is also implemented. This implementation uses code modified from the astropy.timeseries Python package (VanderPlas et al. 2012, 2015).
}
\references{
Baluev, R. V. (2008). Assessing the statistical significance of periodogram peaks.\emph{ Monthly Notices of the Royal Astronomical Society}, \bold{385(3)}, 1279-1285.

Lomb N.R. (1976) Least-squares frequency analysis of unequally spaced data. \emph{Astrophysics and Space Science} \bold{39}:447--462

Press W.H., Teukolsky S.A., Vetterling S.T., Flannery, B.P. (1994) \emph{Numerical recipes in C: the art of scientific computing.}2nd edition. Cambridge University Press, Cambridge, 994pp.

Ruf, T. (1999) The Lomb-Scargle Periodogram in Biological Rhythm Research: Analysis of Incomplete and Unequally Spaced Time-Series. \emph{Biological Rhythm Research} \bold{30}: 178--201. 

Scargle J.D. (1982) Studies in astronomical time series. II. Statistical aspects of spectral analysis of unevenly spaced data. \emph{The Astrophysical Journal} \bold{302}: 757--763.

VanderPlas, J., Connolly, A. Ivezic, Z. & Gray, A. (2012) Introduction to astroML: Machine learning for astrophysics. \emph{Proceedings of the Conference on Intelligent Data Understanding}
        
VanderPlas, J. & Ivezic, Z. (2015) Periodograms for Multiband Astronomical Time Series.\emph{The Astrophysical Jounal} \bold{812.1}:18
}
\author{
Thomas Ruf \email{thomas.p.ruf@me.com} based on code by Press et al (1994).
}

\seealso{
\code{\link{randlsp}}
\code{\link{summary.lsp}}
}
\examples{

# ibex contains an unevenly sampled time series
data(ibex)
lsp(ibex[,2:3],ofac=5)
lsp(ibex$temp,times=ibex$hours,type='period',ofac=5)

# lynx contains evenly sampled data
lsp(lynx)
lynx.spec <- lsp(lynx,type='period',from=2,to=20,ofac=5)
summary(lynx.spec)

# generate unevenly sampled data
time=(runif(200,1,1000))
y=2*cos(time/6)+rnorm(200,0,4)
lsp(y,times=time,ofac=10, to=0.3)

}



\keyword{ ts }

