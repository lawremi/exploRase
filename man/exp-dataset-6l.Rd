\name{exp_dataset}
\alias{exp_dataset}
\title{Get experimental data}
\author{Michael Lawrence <lawremi@iastate.edu>}

\description{
Get experimental data of the specified type out of exploRase
}
\usage{exp_dataset(ent_type = exp_entityType())}
\arguments{
\item{ent_type}{the entity type ("gene", "met", ...) of the data}
}
\value{a GGobi dataset, see rggobi documentation.}
\details{Right now this returns the actual GGobi dataset, for the sake of efficiency,
but after we move to a data.frame implementation of GGobiData, this will
return a frame, so that the dataset cannot be modified}

\examples{}
\keyword{manip}
