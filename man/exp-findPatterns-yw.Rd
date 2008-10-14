\name{exp_findPatterns}
\alias{exp_findPatterns}
\title{Find Patterns}
\author{Michael Lawrence <mflawren@fhcrc.org>}

\description{
Finds patterns in data. Transitions within range of 'fraction' (centered on median)
are considered flat (unchanging). Those below are falling and those above are rising.
}
\usage{exp_findPatterns(data, flat_fraction)}
\arguments{
\item{data}{A data frame of experimental data according to exploRase conventions.}
\item{flat_fraction}{The fraction of transitions considered unchanged, centered on median.}
}
\value{a data frame, with a row for each gene. The first column is the sum of 
  \code{x^i} over all \code{i} from 1 to \code{ncol(data)-1}, where
  \code{x} is 1, 2, or 3, depending on whether the pattern is up, same, or down, 
  respectively, for transition \code{i}.
  The second column contains the magnitude of the pattern.}
\details{}

\examples{}
\keyword{arith}
