\name{exp_loadData}
\alias{exp_loadData}
\title{Load experimental data}
\author{Michael Lawrence <lawremi@iastate.edu>}

\description{
Load experimental data of a specified type into exploRase (and GGobi).
}
\usage{exp_loadData(exp_data, data_name = "expression", ent_type =
"gene", sync = FALSE, add_to_design = !nrow(exp_designFrame(ent_type)))}
\arguments{
\item{exp_data}{a data frame containing experimental data, with 
observations as rows and conditions as columns.}
\item{data_name}{name of the dataset in GGobi}
\item{ent_type}{the entity type ("gene", "met", ...) of the data}
\item{sync}{whether to synchronize the other data models with the 
  experimental data. If \code{sync} is \code{TRUE}, the entity
  information and experimental design tables will be limited to the observations
  and conditions, respectively, in the experimental data. This is
  experimental.}
\item{add_to_design}{whether to add the columns in the dataset as
  conditions in the experimental design, if they are not there already}
}

\details{Organizes experimental data, loads it into GGobi and synchronizes it
with other data (design and entity info)}

\examples{}
\keyword{manip}
