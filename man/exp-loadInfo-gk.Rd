\name{exp_loadInfo}
\alias{exp_loadInfo}
\title{Load entity metadata}
\author{Michael Lawrence <mflawren@fhcrc.org>}

\description{
Loads a data frame of entity metadata into the metadata table of the given type(s).
}
\usage{exp_loadInfo(ent_info, ent_types = "gene", append_col = TRUE, keywords = NULL, update_view = TRUE, sync = FALSE)}
\arguments{
\item{ent_info}{the data frame of entity information}
\item{ent_types}{a single type identifier (applies to all rows) or a}
\item{append_col}{whether the column should be appended or inserted after ID}
\item{keywords}{a vector of identifiers that describe the added metadata}
\item{update_view}{whether the metadata view should be updated}
\item{sync}{whether metadata rows should be filtered out if they don't}
}

\details{The loaded entity metadata is merged with any existing data and the view
is optionally updated to reflect the change. The new set of columns may be
appended to the end or inserted just after the "ID" column.

Metadata for multiple entity types may be added simultaneously,
if \code{ent_types} is a vector with the same number of elements as the
number of rows in \code{ent_info} and specifies the type of the entity
described by each row.}

\examples{}
\keyword{manip}
