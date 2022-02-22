\name{theme_lsp}
\alias{theme_lsp}
\title{lsp theme for ggplot2}
\usage{
theme_lsp(bs=18)
}
\value{
A theme element
}
\description{
Import lsp ggplot2 theme. It builds on theme_bw.
}
\arguments{
  \item{bs}{
  basesize of font
}
}
\examples{
plot(lsp(lynx))+theme_lsp(25)
}