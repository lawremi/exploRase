\name{explorase}
\alias{explorase}
\title{Start exploRase}
\description{The main function of exploRase, normally invoked without arguments, unless
one already has data/metadata in the R session to load.}
\usage{ explorase(exp_data = NULL, entity_info = NULL, design_info = NULL, type = "gene", 
network = NULL, entity_lists = NULL, gobi = ggobi_get(), quit_on_exit = F) }
\arguments{
  \item{exp_data}{Experimental data to load}
  \item{entity_info}{Entity metadata to load}
  \item{design_info}{Experimental design information to load}
  \item{type}{The entity type of the data being loaded}
  \item{network}{A Bioconductor "graph" to load (not yet implemented)}
  \item{entity_lists}{A list of entity lists to load}
  \item{gobi}{The GGobi session to use}
  \item{quit_on_exit}{Whether R should quit when exploRase is exited}
}
\keyword{dynamic}
\author{Michael Lawrence}
