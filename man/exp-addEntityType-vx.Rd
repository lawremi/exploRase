\name{exp_addEntityType}
\alias{exp_addEntityType}
\title{Add an entity type}
\author{Michael Lawrence <mflawren@fhcrc.org>}

\description{
Adds an entity type of the given name with a plural human-readable label.
}
\usage{exp_addEntityType(ent_type, label = paste(ent_type, "s", sep=""))}
\arguments{
\item{ent_type}{The unique identifier of the entity type}
\item{label}{The plural label used for the tab in the metadata notebook.}
}

\details{When an entity type is added to exploRase, a new tab is added to the metadata
notebook with the provided plural label. Data and metadata corresponding to
the type may then be loaded into exploRase.}

\examples{}
\keyword{manip}
