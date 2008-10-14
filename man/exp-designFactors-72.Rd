\name{exp_designFactors}
\alias{exp_designFactors}
\title{Get the exp. design factors}
\author{Michael Lawrence <mflawren@fhcrc.org>}

\description{
Gets a vector of the names of the factors in the experimental design
}
\usage{exp_designFactors(ent_type = exp_entityType(), treatments_only = FALSE)}
\arguments{
\item{ent_type}{the entity type ("gene", "met", etc)}
\item{treatments_only}{if \code{TRUE}, only include treatments (eg genotype), 
  leaving out eg ID and replicate.}
}
\value{A vector of factor names}
\details{}

\examples{}
\keyword{manip}
