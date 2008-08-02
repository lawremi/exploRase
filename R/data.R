# Functions handling experimental data

# Load experimental data
# Load experimental data of a specified type into exploRase (and GGobi).
# 
# Organizes experimental data, loads it into GGobi and synchronizes it 
# with other data (design and entity info)
#
# @arguments a data frame containing experimental data, with
# observations as rows and conditions as columns.
# @arguments name of the dataset in GGobi
# @arguments the entity type ("gene", "met", ...) of the data
# @arguments whether to synchronize the other data models with the
# experimental data. If \code{sync} is \code{TRUE}, the entity
# information and experimental design tables will be limited to the observations
# and conditions, respectively, in the experimental data. This is experimental.
# @arguments whether to add variables in data to experimental design if missing
# @keyword manip
exp_loadData <- function(exp_data, data_name = "expression", ent_type = "gene",
                         sync = FALSE,
                         add_to_design = !nrow(exp_designFrame(ent_type)))
{
  assert(length(dim(exp_data)) == 2, "Experimental data must be bidimensional")
  
  exp_addEntityType(ent_type)
  
  colnames(exp_data) <- trimWhiteSpace(colnames(exp_data))
  rownames(exp_data) <- trimWhiteSpace(rownames(exp_data))
  
  gg <- exp_ggobi()
	gg[data_name] <- exp_data # load data into our ggobi instance
  .exp$setGobiset(gg[data_name], ent_type) # hold the reference
  .exp$setDataset(exp_data, ent_type)
  
	if (sync) { # optionally remove extraneous genes/chips
		syncEntityInfo()
		syncDesignInfo()
	}
	
	# ensure experiments in experimental data are listed
  if (add_to_design)
    addSamples(colnames(exp_data), ent_type)
	
	# ensure entities in experimental data are listed
	addEntities(rownames(exp_data), rep(ent_type, nrow(exp_data)))
		
	# color everything to default gray
	exp_colorEntities(rownames(exp_data), .backgroundColor)
  
  projectStarted()
}

getDatasets <- function() .exp$getDatasets()

# Get experimental data
# Get experimental data of the specified type out of exploRase
# 
# right now this returns the actual GGobi dataset, for the sake of efficiency,
# but after we move to a data.frame implementation of GGobiData, this will
# return a frame, so that the dataset cannot be modified
#
# @arguments the entity type ("gene", "met", ...) of the data
# @value a GGobi dataset, see rggobi documentation.
# @keyword manip
exp_dataset <- function(ent_type = exp_entityType()) getDatasets()[[ent_type]]

# convenience for getting a data.frame of the experimental data
exp_dataFrame <- function(ent_type = exp_entityType())
  as.data.frame(exp_dataset(ent_type))

#################### Data menu callbacks #####################

subset_cb <- 
function(wid, user_data)
{
  subsetDialog()$show()
}

log_transform_cb <- 
function(wid, user_data)
{
}

aggregateReps <-
function(fun, ..., label = deparse(substitute(fun)), ggobi = T, explorase = T)
{
  printTask(paste("Calculating ", label, "s", sep=""))
  means <- applyByReps(fun, ..., progress = 90)
  if (ggobi) {
    printOp("Adding results to GGobi")
    colnames(means) <- paste(colnames(means), label, sep=".")
    sapply(colnames(means), function(mean_name)
      addGGobiVariable(means[,mean_name], mean_name))
  }
  if (explorase) {
    .exp$setDataset(cbind(exp_dataset(), means), exp_entityType())
    ints <- interaction(exp_designFrame(treatment = TRUE))
    design_frm <- exp_designFrame()[!duplicated(ints),]
    #design_frm <- split(exp_designFrame(), exp_designFrame()[,"replicate"])[[1]]
    design_frm[,"ID"] <- colnames(means)
    design_frm[,"replicate"] <- NA
    exp_loadDesign(design_frm)
  }
  finishTask()
}

average_cb <- 
function(wid, user_data)
{
  aggregateReps(mean, na.rm = T)
}

median_cb <- 
function(wid, user_data)
{
  aggregateReps(median, na.rm = T)
}

stddev_cb <-
function(wid, user_data)
{
  aggregateReps(sd, na.rm = T, explorase = F)
}

#################### Utilities ########################

# applies 'fun' to entity values within a replicate set
# the result has a column for each replicate set with rows for each entity
applyByReps <- 
function(fun, ..., ent_type = exp_entityType(), progress = 100) 
{
  trueFactors <- !(exp_designFactors(ent_type) %in% c("ID", "replicate"))
  design <- exp_designFrame(ent_type)
  trueSamples <- !is.na(design[,"replicate"])
  trueDesign <- design[trueSamples,trueFactors]
  data_set <- exp_dataset(ent_type)[,design[trueSamples, "ID"]]
  ints <- interaction(trueDesign)
  levs <- levels(ints)[unique(ints)]
  inc <- progress / length(levels(ints))
  #conds <- levels(ints)[ints]
  #names(conds) <- trueDesign[,"ID"]
  sapply(levs, function(int) {
    printOp("Considering", int)
    result <- apply(data_set[,ints == int,drop=F], 1, fun, ...)
    addProgress(inc)
    result
  })
  #browser()
  #by(t(data_set), ints, function(reps) {
  #  printOp("Considering", conds[rownames(reps)[1]])
  #  result <- apply(reps, 2, fun)
  #  addProgress(inc)
  #  result
  #})
}
