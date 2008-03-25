if(FALSE) {
exp_loadNetwork <- function(sbml, name) {
  # not yet ready for prime-time
  dom <- rsbml_dom(sbml)
  g <- rsbml_graph(sbml)
  species <- species(model(dom))
  reactions <- reactions(model(dom))
  species_match <- match(nodes(g), c(names(species), names(reactions)))
  species_data <- t(sapply(c(species, reactions),
    function(s) c(id = s@id, name = s@name, compartment =
      if (inherits(s, "Species")) s@compartment else NA)
  ))[species_match,]
  layout <- new("RColaLayout", g)
  new("RColaGGobiGUI", layout, as.data.frame(species_data), 
    session = .exp$getGGobi(), factories = biocola_gui_constraint_factories)
}
}
