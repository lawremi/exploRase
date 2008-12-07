# Stuff that interacts with GGobi

# Get the GGobi session
# Gets the GGobi session being used by exploRase. Will restart GGobi if it died
# for some reason.
# @value The GGobi session
# @keyword GUI
# @keyword dynamic
exp_ggobi <- function() {
  if (!valid_ggobi(.exp$getGGobi())) {
    warning("GGobi disappeared, restarting")
    .exp$setGGobi(ggobi())
  }
  .exp$getGGobi()
}

########## Entity coloring stuff

# Coloring entities
# Set the color for the specified entities of the current entity type in GGobi 
# and the metadata table.
# @arguments the entities to color
# @arguments the color index (as interpreted by GGobi) for the entities
# @keyword GUI
# @keyword dynamic
exp_colorEntities <- function(entities = getEntityIds(),
                              color = toGGobiColor(getBrushColor())) {
  gg <- exp_ggobi() 
  sapply(exp_entityTypes(),
         function(ent_type) {
           dataset <- .exp$getGobisets()[[ent_type]]
           if (!is.null(dataset)) {
             ent_ind <- which(rownames(dataset) %in% entities)
             colors <- glyph_color(dataset)
             colors[ent_ind] <- color
             glyph_color(dataset) <- colors
           }
         })
  updateColors(entities)
}

# Updates the Entity Info model with the colors from GGobi
# Eventually callbacks will do this automatically
updateColors <- function(entities = getEntityIds(), types = exp_entityTypes()) {
	models <- getEntityModels()
	sapply(types,
               function(ent_type) {
                 model_entities <- getEntityIds(ent_type)
                 #entities <- entities[entities %in% model_entities]
                 entities_ind <- match(entities, model_entities)
                 entities_ind <- entities_ind[!is.na(entities_ind)]
                 entities <- model_entities[entities_ind]
                 ggobi_colors <- getGGobiEntityColors(entities)
                 colors <- getGGobiColors()[ggobi_colors]
                 if (length(colors) > 0)
                   models[[ent_type]][entities_ind,"color"] <- unlist(colors)
                 propagateEntityInfo(ent_type, entities_ind)
               })
}

getGGobiColors <- function() .exp$getGGobiColors()

# FIXME: using dist() here would be faster
toGdkColors <- function(colors)
{
	text_colors <- lapply(colors, function(color) {
		# to hex string representation
		paste("#", paste(as.raw(round(color * 255)), collapse=""), sep="")
	})
  color_table <- read.table(ggFile("colors", "rgb.txt"), header=T)
  names(text_colors) <- sapply(colors, function(color) {
    as.character(color_table[sort.list(apply(abs(t(255*color - t(color_table[,1:3]))),
      1, sum))[1],4])
  })
  text_colors
}

# convert between rgb color string and GGobi color id
toGGobiColor <- function(color)
{
	match(color, getGGobiColors())
}

getGGobiEntityColors <- function(entities = getEntityIds()) {
  gg <- exp_ggobi()
  colors <- rep(.backgroundColor, length(entities))
  names(colors) <- entities
  sapply(exp_entityTypes(), function(ent_type) {
    dataset <- .exp$getGobisets()[[ent_type]]
    if (!is.null(dataset)) {
      ids <- rownames(dataset)
      ggobi_ind <- match(entities, ids)
      colors[ids[ggobi_ind]] <<- glyph_color(dataset)[ggobi_ind]
    }
  })
  colors
}

addGGobiVariable <- function(values, var_name, ent_type = exp_entityType()) {
  results <- unlist(values)
  dataset <- .exp$getGobisets()[[ent_type]]
  dataset[[var_name]] <- values
  #if (var_name %in% colnames(dataset))
  #  ggobi_data_set_variables(dataset, values, var_name)
  #else addVariable(dataset, values, var_name)
}

createGGobiDisplay <- function(pmode)
{
  d <- .exp$getGobisets()[[exp_entityType()]]
  display(d, pmode, list(X=exp_designSelection()))
}

parcoords_cb <- function(wid, data)
{
  createGGobiDisplay("Parallel Coordinates Display")
}
scatmat_cb <- function(wid, data)
{
  createGGobiDisplay("Scatterplot Matrix")
}

MAplot_cb <- function(wid, data)
{
  types <- exp_entityType()
  keyword <- NULL
  pmode <- "XY Plot"
  samples <- exp_designSelection()
  assert(length(samples) == 2, "Please select two samples")
  samples <- sort(samples)
  dataset <- exp_dataset()
  reg_y <- dataset[,samples[2]]
  reg_x <- dataset[,samples[1]]
  diff <- exp_calcDiff(reg_x, reg_y)
  mean <- (reg_x+reg_y)/2
  mean1 <- createVarName(samples, "mean")
  diff1 <- createVarName(samples, "diff")
  addResultColumn(mean, mean1, types, keyword)
  addGGobiVariable(mean, mean1, types)
  addResultColumn(diff, diff1, types, keyword)
  addGGobiVariable(diff, diff1, types)
  d <- .exp$getGobisets()[[exp_entityType()]]
  display(d, pmode, list(X=mean1, Y=diff1))
  finishTask()
}
