\name{exp_showPatterns}
\alias{exp_showPatterns}
\title{Show patterns}
\author{Michael Lawrence <mflawren@fhcrc.org>}

\description{
Show the calculated patterns in the GUI
}
\usage{exp_showPatterns(patterns, desc, samples = exp_designSelection())}
\arguments{
\item{patterns}{a data frame, with a row for each gene and the first column being
  the pattern codes and the second the magnitude of the pattern 
  (as returned by \code{\link{exp_findPatterns}}).}
\item{desc}{a description of the patterns, for labeling them in the GUI}
\item{samples}{the samples involved in the calculation, for labeling}
}

\details{}

\examples{}
\keyword{dynamic}
