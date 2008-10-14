\name{exp_showResults}
\alias{exp_showResults}
\title{Show analysis results}
\author{Michael Lawrence <mflawren@fhcrc.org>}

\description{
Add a column of analysis results to the exploRase table and the GGobi dataset.
}
\usage{exp_showResults(results, label, sublabels = "", types = exp_entityType(), keyword = NULL, explorase = T, ggobi = T)}
\arguments{
\item{results}{the data frame of results. To include results for multiple
  entity types at once, the result for each type should be concatenated in the
  same order as the \code{types} parameter.}
\item{label}{a root label for the result (like the type of analysis)}
\item{sublabels}{other labels concatenated to the root (such as the conditions involved)}
\item{types}{the entity type(s) of the data from which this result was derived.}
\item{keyword}{a keyword identifying this result, for use in context-sensitive help}
\item{explorase}{whether to show the result in the explorase table}
\item{ggobi}{whether to add the result to the GGobi dataset}
}

\details{}

\examples{}
\keyword{dynamic}
\keyword{manip}
