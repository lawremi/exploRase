\name{bioc_integration}
\alias{exp_expressionSet}
\alias{exp_featureData}
\alias{exp_loadExpressionSet}
\alias{exp_phenoData}
\title{ ExpressionSet Integration }
\description{
  Functions loading and retrieving Bioconductor ExpressionSet objects to
  and from exploRase.
}
\usage{
exp_loadExpressionSet(exprset, type = exp_entityType())
exp_phenoData(type = exp_entityType())
exp_featureData(type = exp_entityType())
exp_expressionSet(type = exp_entityType())
}
\arguments{
  \item{exprset}{ The \code{ExpressionSet} to load. }
  \item{type}{ The type of the biological entity (e.g. gene). }
}
\details{
  To load a \code{ExpressionSet} instance into exploRase, call
  \code{exp_loadExpressionSet}.
  
  The function \code{exp_expressionSet} retrieves the
  \code{ExpressionSet} representing the data stored in exploRase for the
  given entity type.  \code{exp_phenoData} and \code{exp_featureData}
  retrieve only the \code{phenoData} and \code{featureData} components,
  respectively.
}
\value{
  For \code{exp_expressionSet}, an \code{ExpressionSet}.
  For \code{exp_phenoData}, an \code{AnnotatedDataFrame} representing
  the phenotype data (i.e. experimental design, see
  \code{\link{exp_designFrame}}).
  For \code{exp_featureData}, an \code{AnnotatedDataFrame} representing
  the feature annotations (i.e. the entity info, see
  \code{\link{exp_entityFrame}}).
}
\author{ Michael Lawrence }
\seealso{ \code{\link{explorase}} for loading data and starting
  exploRase in one call. \code{exp_designFrame}, \code{exp_entityFrame},
  \code{exp_dataset}, etc for retrieving data from exploRase. }
\examples{
}
\keyword{ manip }
