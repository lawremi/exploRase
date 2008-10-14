\name{exp_loadLists}
\alias{exp_loadLists}
\title{Load entity lists}
\author{Michael Lawrence <mflawren@fhcrc.org>}

\description{
Loads a list of entity lists (matrices) into exploRase
}
\usage{exp_loadLists(ent_lists)}
\arguments{
\item{ent_lists}{a list of entity lists (1 or 2 column matrices)}
}

\details{An entity list matrix may have one or two columns. The last column specifies
the entity ID's and its name is the name of the entity list.
If there are two columns, the first specifies the type
of each entity, allowing entity lists holding entities of different types.}

\examples{}
\keyword{manip}
