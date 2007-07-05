\name{exp_findPatterns}
\alias{exp_findPatterns}
\title{Find Patterns}
\author{Michael Lawrence <lawremi@iastate.edu>}

\description{
Finds patterns in experimental data
}
\usage{exp_findPatterns(data)}
\arguments{
\item{data}{A data frame of experimental data according to exploRase conventions.}
}
\value{a data frame, with a row for each gene. The first column is the sum of 
  \code{x^i} over all \code{i} from 1 to \code{ncol(data)-1}, where
  \code{x} is 1, 2, or 3, depending on whether the pattern is up, same, or down, 
  respectively, for transition \code{i}.
  The second column contains the magnitude of the pattern.}
\details{}

\examples{}
\keyword{arith}
