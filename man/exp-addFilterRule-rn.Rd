\name{exp_addFilterRule}
\alias{exp_addFilterRule}
\title{Add a filter rule}
\author{Michael Lawrence <mflawren@fhcrc.org>}

\description{
Adds a filter rule to the exploRase filter model for the given entity type
}
\usage{exp_addFilterRule(id, column, op, expr, active = T, ent_type = exp_entityType())}
\arguments{
\item{id}{A unique identifier for the filter rule}
\item{column}{The metadata column checked by the rule}
\item{op}{The operator (<, >, ==, etc) used for checking the values}
\item{expr}{The right hand expression against which the values are checked}
\item{active}{Whether the rule should be immediately active}
\item{ent_type}{The entity type of the metadata being filtered}
}

\details{}

\examples{}
\keyword{manip}
