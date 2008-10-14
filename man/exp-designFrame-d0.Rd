\name{exp_designFrame}
\alias{exp_designFrame}
\title{Get the exp. design}
\author{Michael Lawrence <mflawren@fhcrc.org>}

\description{
Gets a data frame containing the experimental design information for the given entity type.
}
\usage{exp_designFrame(ent_type = exp_entityType(), treatments_only = FALSE)}
\arguments{
\item{ent_type}{the entity type ("gene", "met", etc)}
\item{treatments_only}{if \code{TRUE}, only include treatment columns (eg genotype), 
  leaving out eg ID and replicate.}
}
\value{A data frame with conditions as rows and factors as columns.}
\details{}

\examples{}
\keyword{manip}
