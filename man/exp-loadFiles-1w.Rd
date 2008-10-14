\name{exp_loadFiles}
\alias{exp_loadFiles}
\title{Load files}
\author{Michael Lawrence <mflawren@fhcrc.org>}

\description{
Loads a set of files into exploRase
}
\usage{exp_loadFiles(filenames, data_type, entity_type = "gene")}
\arguments{
\item{filenames}{filenames The paths to the files to load}
\item{data_type}{The data type ("data", "design", "info", "list")}
\item{entity_type}{The entity type (by default: "gene", "met", "prot"),
only used if \code{data_type} is provided.}
}

\details{If the \code{data_type} is specified, it is assumed that all the files
are of the given \code{data_type} and \code{entity_type}. Otherwise,
the types are autodetected based on file extensions.}

\examples{}
\keyword{IO}
