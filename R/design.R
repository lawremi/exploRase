
###################### The simple design view #######################

designView <- function(model) {
   view <- gtkTreeView(model)
   col_names <- "ID"
   textColumnsToView(view, col_names)
   view$getSelection()$setMode("multiple")
   view
}

getDesignView <- function() .exp$getDesignView()

####################### Design details view #######################

designDetailsButton_cb <- function(w, u = NULL)
{
   designViewWindow()$show()
}

designViewWindow <-function(exp.info, parent = .exp$getMainWindow())
{
  expViewWindow <- gtkWindow("toplevel", show = F)
  expViewWindow$setTitle("exploRase : Experimental design details")
  expViewWindow$setTransientFor(parent)
  expViewWindow$setDestroyWithParent(TRUE)
  expViewWindow$setDefaultSize(500,250)
  expViewWindow$setResizable(TRUE)

 #-----  Exp List  -----#
  expView <- gtkTreeView(getDesignModel())
  expView$setRulesHint(TRUE)
  textColumnsToView(expView, exp_designFactors())
  configureViewColumns(expView)
  
  # scrollwindow
  expscroll <- gtkScrolledWindow(NULL, NULL)
  expscroll$add(expView)
  expscroll$setBorderWidth(7)

  # layout
  layout <- gtkVBoxNew(FALSE,7)
  layout$setBorderWidth(7)
  layout$packStart(expscroll,TRUE,TRUE,0)
  expViewWindow$add(layout)
  
  expViewWindow
}

##################### The design model ###################

# Creates a model for storing the experiment information.
# It contains one built in column: id.
# If 'design_info' is given, it is loaded into the model
designModel <- 
function(design_info = NULL)
{
  cols <- c("ID" = "character")
  createDataModel(cols, design_info)
}

# adds some samples (microarray chips, metabolite profiles, etc) if they do not yet exist
addSamples <- function(samples, type) {
  model <- getDesignModel(type)
  new <- which(!(samples %in% t(model[,"ID"])))
  if (length(new) > 0) {
    sample_frame <- data.frame(ID=samples[new])
    exp_loadDesign(sample_frame, type)
  }
}

# remove samples that do not exist
syncDesignInfo <- function() {
  allowed <- unique(sapply(getDatasets(), function(dataset) {
    colnames(dataset)
  }))
  sapply(getDesignModels(), function(model) {
    samples <- model[,"ID"]
    allowed_samples <- t(samples) %in% allowed
    model$setFrame(model[allowed_samples,])
  })
}

# Load experimental design
# Loads a matrix describing the experimental conditions
#
# @arguments a data frame with conditions for rows and factors for cols
# @arguments the type of entity to which this design applies. This links
# the design to the experimental data and entity information.
# @keyword manip
exp_loadDesign <- function(design_info, ent_type = exp_entityType()) {
  model <- getDesignModel(ent_type)
  exp_addEntityType(ent_type)
  # force first column name to be ID
  #if (colnames(design_info)[1] != "ID")
  #  design_info <- cbind(ID=rownames(design_info), design_info)
  #design_model <- designModel(design_info)
  #.exp$setDesignModel(design_model, ent_type)
  #getDesignView()$setModel(design_model)
  if (is(design_info, "AnnotatedDataFrame")) {
    pdata <- pData(design_info)
    design_info <- cbind(ID = rownames(pdata), pdata)
  }

  ## strip extra ID column
  id_dup <- apply(design_info, 2, function(col) all(col == design_info[,1]))
  id_dup[1] <- FALSE
  id_dup[is.na(id_dup)] <- FALSE
  design_info <- design_info[,!id_dup,drop=FALSE]

  mergeInfo(model, design_info)

  ## try to add replicate column if missing and we have at least one factor
  factors <- exp_designFactors(ent_type, TRUE)
  if (!("replicate" %in% colnames(model)) && length(factors)) {
    int <- interaction(model[,factors,drop=FALSE])
    model[,"replicate"] <- unsplit(lapply(table(int), seq_len), int)
  }

  projectStarted()
	#if (sync)
  #  syncDesignInfo()
}

# Gets the design info model for the given type
getDesignModel <- function(ent_type = exp_entityType()) getDesignModels()[[ent_type]]
getDesignModels <- function() .exp$getDesignModels()

# Get the exp. design
# Gets a data frame containing the experimental design information for the given entity type.
# @arguments the entity type ("gene", "met", etc)
# @arguments if TRUE, only include treatments (eg genotype), leaving out eg ID and replicate.
# @value A data frame with conditions as rows and factors as columns.
# @keyword manip
exp_designFrame <- function(ent_type = exp_entityType(), treatments_only = FALSE)
{
  df <- as.data.frame(getDesignModels()[[ent_type]])
  if (treatments_only)
    df <- df[,!(colnames(df) %in% c(".visible", "ID", "replicate")),drop=FALSE]
  df
}

# Get the exp. design factors
# Gets a vector of the names of the factors in the experimental design
# @arguments the entity type ("gene", "met", etc)
# @arguments if TRUE, only include treatments (eg genotype), leaving out eg ID and replicate.
# @value A vector of factor names
# @keyword manip
exp_designFactors <- function(ent_type = exp_entityType(), treatments_only = FALSE)
  colnames(exp_designFrame(ent_type, treatments_only))

# Get selected conditions
# Gets a vector of the names of the selected conditions in the condition list.
# @value A vector of condition names
# @keyword manip
exp_designSelection <- function() getSelectedData(getDesignView(), "ID")
