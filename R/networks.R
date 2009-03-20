if (FALSE) {
exp_loadNetwork <- function(sbml, name = deparse(substitute(sbml))) {
  ## not yet ready for prime-time
  require(rsbml)
  require(Rgraphviz)
  dom <- sbml
  g <- rsbml_graph(sbml)
  species <- species(model(dom))
  reactions <- reactions(model(dom))
  species_match <- match(nodes(g), c(names(species), names(reactions)))
  make_species_record <- function(s)
    c(id = s@id, name = s@name,
      compartment = if (inherits(s, "Species")) s@compartment else NA)
  species_data <- t(sapply(c(species, reactions), make_species_record))
  species_data <- as.data.frame(species_data[species_match,])
  rownames(species_data) <- species_data$id
  g <- layoutGraph(g)
  species_data$X <- g@renderInfo@nodes$nodeX
  species_data$Y <- g@renderInfo@nodes$nodeY
  gg <- .exp$getGGobi()
  gg[name] <- species_data
  d <- gg[name]
  edges <- g
  edges(gg) <- edges
}
}

if(FALSE) {
  exp_loadNetwork <- function(sbml, name) {
                                        # not yet ready for prime-time
    dom <- rsbml_dom(sbml)
    g <- rsbml_graph(sbml)
    species <- species(model(dom))
    reactions <- reactions(model(dom))
    species_match <- match(nodes(g), c(names(species), names(reactions)))
    species_data <- t(sapply(c(species, reactions),
                             function(s)
                             c(id = s@id, name = s@name,
                               compartment = if (inherits(s, "Species"))
                               s@compartment else NA)
                             ))[species_match,]
    layout <- new("RColaLayout", g)
    new("RColaGGobiGUI", layout, as.data.frame(species_data), 
        session = .exp$getGGobi(), factories = biocola_gui_constraint_factories)
  }
}
