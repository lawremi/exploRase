\name{exp_dataset}
\alias{exp_dataset}
\alias{exp_dataFrame}
\title{Get experimental data}
\author{Michael Lawrence <mflawren@fhcrc.org>}

\description{
Get experimental data of the specified type out of exploRase
}
\usage{
exp_dataset(ent_type = exp_entityType())
exp_dataFrame(ent_type = exp_entityType())
}
\arguments{
\item{ent_type}{the entity type ("gene", "met", ...) of the data}
}
\value{
  For \code{exp_dataset}, a GGobi dataset, see rggobi documentation.
  For \code{exp_dataFrame}, a data.frame coerced from the result of
  \code{exp_dataset}.
}
\details{The result of \code{exp_dataset} allows one to directly
  manipulate the GGobi dataset. If only a \code{data.frame} is required,
  \code{exp_dataFrame} is a more convenient path.}

\examples{}
\keyword{manip}
