

################## Entity types ##################

getEntityTypes <- function() .exp$getEntityTypes()
# Get all entity types
# Retrieves the identifiers for all entity types in exploRase.
# @value Vector of entity type names
# @keyword manip
exp_entityTypes <- function() names(getEntityTypes())
# Get the current entity type
# Retrieves the entity type with its tab selected in the metadata notebook.
# @value Name of the current entity type
# @keyword GUI
exp_entityType <- function() .exp$getEntityType()

entityBookSwitchPage_cb <- function(notebook, page_wid, page_num)
{
  setEntityType(exp_entityTypes()[page_num+1], F)
}

# Add an entity type
# Adds an entity type of the given name with a plural human-readable label.
#
# When an entity type is added to exploRase, a new tab is added to the metadata
# notebook with the provided plural label. Data and metadata corresponding to 
# the type may then be loaded into exploRase.
#
# @arguments The unique identifier of the entity type. Should match
# the file extension for data corresponding to the type.
# @arguments The plural label used for the tab in the metadata notebook.
# @keyword manip
# @keyword GUI
exp_addEntityType <- function(ent_type, label = paste(ent_type, "s", sep="")) {
  if (ent_type %in% names(getEntityTypes()))
    return()
  .exp$addEntityType(ent_type, label)
  .exp$setDesignModel(designModel(), ent_type)
  entModel <- entityModel()
  .exp$setEntityModel(entModel, ent_type)
  entView <- entityView(entModel)
  .exp$setEntityView(entView, ent_type)
  .exp$setFilterModel(filterModel(), ent_type)
  filtView <- filterView(ent_type)
  columnView <- columnView(entView)
  searchBar <- searchBar(entView)
  .exp$getEntityBook()$appendPage(entityPage(entView, filtView, columnView, searchBar), 
    gtkLabel(label))
}

setEntityType <- function(ent_type, turn_page = TRUE) {
  exp_addEntityType(ent_type) # make sure it is a valid type
  w <- match(ent_type, exp_entityTypes())
  book <- .exp$getEntityBook()
  if (turn_page && w-1 != book$getCurrentPage())
    book$setCurrentPage(w-1)
  getDesignView()$setModel(getDesignModel(ent_type))
  .exp$setEntityType(ent_type)
}

# try to guess entity type from ID
# currently this just returns "gene" .. don't know how this will work, if at all
inferEntityType <- function(ids) "gene"

################## The entity info model ###################

# Gets the entity ids of the currently selected type
getCurrentEntityIds <- function() getEntityIds(exp_entityType())

# more specific wrapper for inserting analysis results in the gene info view
addResultColumn <- function(results, results_name, types, keyword = NULL)
{
	results <- as.data.frame(results)
	#rownames(results) <- rownames(.expData)
	#results <- as.data.frame(results[getEntityIds(types),])
  rows <- as.vector(sapply(getDatasets()[types], rownames))
	results <- results[match(getEntityIds(types), rows),,drop=F]
  colnames(results)[1] <- results_name
	addInfoColumns(results, types, F, keyword)
}

# adds some entities if they do not yet exist
# at first, we wanted to speed things up by doing this from scratch, but 
# now it's just done with exp_loadInfo()
addEntities <- function(ents, types) {
	new_ents <- which(!(ents %in% getEntityIds()))
	nnew <- length(new_ents)
	if (nnew > 0) {
    if (missing(types))
      types <- inferEntityType(ents[new_ents])
    else types <- types[new_ents]
		exp_loadInfo(data.frame(ID = ents[new_ents]), types)
	}
}

# below are some functions for synchronizing design and entity info with
# the experimental data. that is, they remove chips/genes that are not
# represented in the experimental data. this behavior may be toggled by the user.

# removes entities that do not exist in experimental data
syncEntityInfo <- function() {
	allowed <- unique(sapply(getDatasets(), function(dataset) {
		rownames(dataset)
	}))
	sapply(getEntityModels(), function(model) {
		ids <- getEntityIds(model)
		allowed_ents <- ids %in% allowed
    model$setFrame(model[allowed_ents,])
	})
}

################### The entity info view ####################

entityPage <- function(entView, filterView, columnView, searchBar) 
{
  vbox <- gtkVBox(FALSE, 3)
  entityScroll <- scrollView(entView)
  filter <- gtkExpander("Filter")
  filter$add(filterView)
  vbox$packStart(filter, FALSE, FALSE, 0)
  columns <- gtkExpander("Hide/Show Columns")
  columns$add(scrollView(columnView))
  vbox$packStart(columns, FALSE, FALSE, 0)
  vbox$packStart(searchBar, FALSE, FALSE, 0)
  vbox$packStart(entityScroll, TRUE, TRUE, 0)
  vbox
}

# highlights (selects) rows in an entity info view
highlightEntities <- function(ents, ent_type, scroll = FALSE) {
  entView <- getEntityView(ent_type)
  sel <- entView$getSelection()
  entModel <- getEntityModel(ent_type)
  # Mapping in R is likely faster than asking GTK to map each one
  #mapping <- rep(NA, ncol(entModel))
  #visible <- entModel[,".visible"]
  #mapping[visible] <- 1:length(which(visible))
	#mapped_ind <- mapping[ents]
  mapped_ind <- match(entModel[ents,"ID"], entView$getModel()[,"ID"])
  # End Mapping
  mapped_ind <- mapped_ind[!is.na(mapped_ind)]
	sapply(mapped_ind, function(ind) 
    sel$selectPath(gtkTreePathNewFromIndices(ind-1)))
  if (length(mapped_ind) && scroll)
    entView$scrollToCell(gtkTreePathNewFromIndices(mapped_ind[1]-1), use.align=TRUE)
}

# builds a entity info view for a model that must adhere to the same format
# as the model generated by entityModel(). That is, the model must
# have at least 4 columns, with the first as a "global filter" and the second
# holding colors. The third column holds the lists and the fourth the ID 
# the view has ruled rows,
# the headers are clickable, and multiple selections are allowed.
entityView <- function(model)
{
	col_names <- c("lists", "ID", colnames(model)[userCols(model)])
	#filter_model <- gtkTreeModelFilterNew(model)
  #filter_model$setVisibleColumn(0)
	#ent_view <- gtkTreeView(filter_model)
  entView <- gtkTreeView(rGtkDataFrame(as.data.frame(model)))
	entView$setRulesHint(TRUE)
	entView$setHeadersClickable(TRUE)
	entView$getSelection()$setMode("multiple")
  entView$setFixedHeightMode(T)
	textColumnsToView(entView, col_names)
  colorCol <- gtkTreeViewColumn("color", gtkCellRendererText(), 
    background = match("color", colnames(model))-1)
  colorCol$setSizing("fixed")
  colorCol$setFixedWidth(60)
  entView$insertColumn(colorCol, 0)
	entView$setEnableSearch(TRUE)
	entView
}

# Displaying data in the views

maxTextWidth <- function(entView, text)
{
  str <- text[which.max(sapply(text, nchar))]
  layout <- entView$createPangoLayout(str)
  extents <- layout$getPixelExtents()
  extents$ink_rect$width
}

updateEntityView <- function(type, old_names, append_col = T, keywords = NULL) {
	ent_view <- getEntityViews()[[type]]
	model <- getEntityModel(type)
	new_vis <- userCols(model)
	new_names <- colnames(model)
	#view$setModel(model)
	new_ind <- new_vis[!(new_names[new_vis] %in% old_names)]
	if (length(new_ind) > 0) {
		insert <- 3 # insert after the "id"
		if (append_col) insert <- -1 # or append
		cols <- textColumnsToView(ent_view, new_names[new_ind], insert = insert)
		sapply(1:length(cols), function(col)
			configureViewColumn(cols[[col]], new_ind[col], T, new_names[new_ind[col]], keywords[col]))
	}
}

getEntityViews <- function() .exp$getEntityViews()
getEntityView <- function(ent_type = exp_entityType()) 
  getEntityViews()[[ent_type]]

# Get selected entities
# Gets the ID's of the entities selected in the metadata table of the given type.
# @arguments The entity type for which the selected is retrieved
# @value The entity ID's
# @keyword GUI
exp_entitySelection <- function(ent_type = exp_entityType()) 
  getSelectedData(getEntityView(ent_type), "ID")

############################## Entity info model ###########################

# Load entity metadata
# Loads a data frame of entity metadata into the metadata table of the given type(s).
# 
# The loaded entity metadata is merged with any existing data and the view
# is optionally updated to reflect the change. The new set of columns may be 
# appended to the end or inserted just after the "ID" column.
#
# Metadata for multiple entity types may be added simultaneously,
# if \code{ent_types} is a vector with the same number of elements as the
# number of rows in \code{ent_info} and specifies the type of the entity
# described by each row.
# 
# @arguments the data frame of entity information
# @arguments a single type identifier (applies to all rows) or a 
# vector specifying the type of entity described by each row.
# @arguments whether the column should be appended or inserted after ID
# @arguments a vector of identifiers that describe the added metadata
# for the purposes of context-sensitive help.
# @arguments whether the metadata view should be updated
# @arguments whether metadata rows should be filtered out if they don't 
# exist in the experimental data
# @keyword manip
# @keyword GUI
exp_loadInfo <- 
function(ent_info, ent_types = "gene", append_col = TRUE, keywords = NULL,
         update_view = TRUE, sync = FALSE) 
{
  if (is(ent_info, "AnnotatedDataFrame")) {
    pdata <- pData(ent_info)
    if ("ID" %in% colnames(pdata)) ## ID column must be first
      ent_info <- cbind(ID = pdata$ID,
                        pdata[,colnames(pdata) != "ID", drop=FALSE])
    else ent_info <- cbind(ID = rownames(pdata), pdata)
  }
  types <- unique(ent_types)
  models <- getEntityModels()
  sapply(types, exp_addEntityType)
  sapply(types[types %in% names(models)], function(ent_type) {
    old_names <- colnames(models[[ent_type]])
    if (length(ent_types) == 1)
      info <- ent_info
    else info <- ent_info[which(ent_type == ent_types),]
    loadInfoSubset(info, ent_type, sync)
    if(update_view) {
      updateEntityView(ent_type, old_names, append_col, keywords)
      updateFilterColumnBox(ent_type)
    }
  })
  projectStarted()
}

# loads entity info into the given entity info data model
# treats the first column as the key for identifying the unique entities
# merges the new info into the data model
loadInfoSubset <- function(ent_info, ent_type, sync = F) {
  model <- getEntityModel(ent_type)
  
  def <- list(.visible = T, lists = as.character(NA),
              color = getGGobiColors()[.backgroundColor])
  def[getListNames()] <- F
  mergeInfo(model, ent_info, def)
	
  updateModelListColumn(model)
  propagateEntityInfo(ent_type)
  
  ## If asked, hide entities that are not in experimental data
  if (sync)
    syncEntityInfo()
  
  TRUE # success
}

# Creates an entity info model with built in columns for the global filter,
# color names, colors, and ids. If additional columns are contained in the
# optional 'entity_info' data frame, these are also included
# if provided, 'entity_info' is then loaded into the model
entityModel <- 
function(entity_info = NULL)
{
  cols <- c(".visible" = "logical", "color" = "character", "ID" = "character", 
    "lists" = "character")
	createDataModel(cols, entity_info)
}

updateModelListColumn <- function(model, cols = entityListCols(model))
{
  vals <- NULL
  rows <- FALSE
  if (length(cols) > 0) {
    for (ent_col in cols)
      rows <- rows | model[,ent_col]
    lists <- colnames(model)[cols]
    vals <- apply(model[rows,cols,drop=F], 1, function(ent_row) { 
      paste(lists[ent_row], collapse=", ")
    })
  }
  if (length(vals) > 0 && any(rows))
    model[rows,"lists"] <- vals
  which(rows)
}

unloadInfo <- function(ent_type = exp_entityType()) {
	ent_view <- getEntityView(ent_type)
  model <- ent_view$getModel()
  cols <- c("ID", colnames(model)[userCols(getEntityModel(ent_type))])
  cols <- cols[sapply(ent_view$getColumns()[-(1:2)], gtkTreeViewColumnGetVisible)]
	model[,cols]
}

# Get entity metadata
# Retrieves the entity metadata table for the given type as a data frame
# @arguments the entity type for which the metadata is retrieved
# @keyword manip
exp_entityFrame <- function(ent_type = exp_entityType())
  as.data.frame(getEntityModel(ent_type))

getNumEntities <- function(types = exp_entityTypes()) {
	sapply(types, function(ent_type) length(getEntityIds(ent_type)))
}
getEntityIds <- function(types = exp_entityTypes()) {
	models <- getEntityModels()
	unlist(sapply(types, function(ent_type) models[[ent_type]][,"ID"]))
}

getEntityModels <- function() .exp$getEntityModels()

# Gets the entity info model for the given type
getEntityModel <- function(ent_type = exp_entityType()) 
  getEntityModels()[[ent_type]]


### GUI for hiding and showing columns in the entity info view ###

columnModel <- function(ent_view)
{
  cols <- ent_view$getColumns()
  model <- gtkListStore("logical", "character", "GtkTreeViewColumn")
  model_row <- function(col) {
    iter <- model$append()$iter
    model$set(iter, 0, col$getVisible(), 1, col$getTitle(), 2, col)
  }
  sapply(cols, model_row)
  model
}
updateColumnView <- function(col_view, ent_view)
{
  col_view$setModel(columnModel(ent_view))
}
visibleRendererToggled_cb <- function(renderer, path_str, col_view)
{
  model <- col_view$getModel()
  path <- gtkTreePathNewFromString(path_str)
  iter <- model$getIter(path)$iter
  active <- model$get(iter, 0)[[1]]
  col <- model$get(iter, 2)[[1]]
  col$visible <- !active
  model$set(iter, 0, !active)
  model$rowChanged(path, iter)
}
#debug(visibleRendererToggled_cb)

columnView <- function(ent_view)
{
  # the column view
  model <- columnModel(ent_view)
	colView <- gtkTreeView(model)
	colView$setRulesHint(TRUE)
	textColumnsToView(colView, "Column Name", 1)
  renderer <- gtkCellRendererToggle()
  renderer$activatable <- TRUE
  gSignalConnect(renderer, "toggled", visibleRendererToggled_cb, colView)
  visibleCol <- gtkTreeViewColumn("Visible", renderer, active = 0)
  colView$insertColumn(visibleCol, 0)
	colView$setEnableSearch(TRUE)
	colView$setSearchColumn(1) # search on the name
  handler <- gSignalConnect(ent_view, "columns-changed", updateColumnView, colView, 
    user.data.first = TRUE)
  gSignalConnect(colView, "destroy", 
    function(col_view) gSignalHandlerDisconnect(ent_view, handler))
  updateColumnView(colView, ent_view)
  colView$setSizeRequest(-1, 200)
  colView
}

############## Quick Search Bar ################

updateSearchComboBox <- function(combo, ent_view)
{
  active <- combo$active
  combo$setModel(searchComboModel(ent_view))
  combo$active <- active
}
searchComboModel <- function(ent_view)
{
  columns <- ent_view$getColumns()
  #visible <- columns[sapply(columns, gtkTreeViewColumnGetVisible)]
  column_names <- sapply(columns, gtkTreeViewColumnGetTitle)
  df <- data.frame("Column" = column_names, stringsAsFactors=FALSE)
  # only columns with view titles that match model names are included
  # FIXME: somehow include color with these?
  df <- df[(df[,1] %in% colnames(ent_view$getModel())) & df[,1] != "color",,drop=F]
  rGtkDataFrame(df)
}
searchComboChanged_cb <- function(combo, ent_view)
{
  #active <- combo$active+2
  #col <- ent_view$getColumns()[[active]]
  ind <- match(combo$getModel()[combo$active+1,1], colnames(ent_view$getModel()))
  ent_view$setSearchColumn(ind-1)
}
searchBar <- function(ent_view)
{
  columnModel <- searchComboModel(ent_view)
  columnBox <- gtkComboBox(columnModel)
  renderer <- gtkCellRendererText()
  columnBox$packStart(renderer)
  columnBox$setAttributes(renderer, "text" = 0)
  handler <- gSignalConnect(ent_view, "columns-changed", updateSearchComboBox, 
    columnBox, user.data.first = TRUE)
  gSignalConnect(columnBox, "destroy", 
    function(columnBox) gSignalHandlerDisconnect(ent_view, handler))
  gSignalConnect(columnBox, "changed", searchComboChanged_cb, ent_view)
  columnBox$setActive(1)
  hbox <- gtkHBox(FALSE, 5)
  hbox$packStart(gtkLabel("Quick Search:"), FALSE, FALSE, 0)
  hbox$packStart(columnBox, FALSE, FALSE, 0)
  if (is.null(gtkCheckVersion(2,10,0))) {
    entry <- gtkEntry()
    ent_view$setSearchEntry(entry)
    hbox$packStart(entry, TRUE, TRUE, 0)
  }
  hbox
}

### utilities ###

propagateEntityInfo <- function(type = exp_entityType(), rows)
{
  # no longer make the filter 'live'
  #if (refreshFilter(type)) # filter changed, so everything is updated
  #  return()
  entView <- getEntityView(type)
  entModel <- getEntityModel(type)
  if (missing(rows)) { # many/all rows changed, just replace model
    # make sure search column is not lost here
    search_col <- entView$getSearchColumn()
    entView$setModel(rGtkDataFrame(as.data.frame(entModel[entModel[,1]])))
    entView$setSearchColumn(search_col)
  }
  else if (length(rows) > 0) { # map the raw indices to filtered indices
    viewModel <- entView$getModel()
    ents <- entModel[rows,"ID"]
    ent_matching <- match(ents, viewModel[,"ID"])
    matched_ents <- !is.na(ent_matching)
    viewModel[ent_matching[matched_ents],] <- entModel[rows[matched_ents],]
    #mapping <- rep(NA, nrow(entModel))
    #mapping[entModel[,1]] <- 1:nrow(entView$getModel())
    #matched_rows <- rows[!is.na(mapping[rows])]
    #viewModel[mapping[matched_rows],] <- entModel[matched_rows,]
  }
}

# adds the columns of a data frame to the entity models
addInfoColumns <- function(new_data, types = exp_entityTypes(), append_col = T, 
    keywords = NULL, update_view = T) {
	new_data <- as.data.frame(new_data)
  exp_loadInfo(cbind(ID = getEntityIds(types), new_data), rep(types, getNumEntities(types)),
    append_col, keywords, update_view)
  # ensure that all specified types get a column, even if empty
  dummy <- empty_data_frame(sapply(new_data, mode))
  sapply(getEntityModels()[types][getNumEntities(types) == 0], function(model) {
    missing_cols <- !(colnames(dummy) %in% colnames(model))
    if (any(missing_cols))
      model$appendColumns(dummy[missing_cols])
  })
}

# NOTE: the following three functions produce R indices (starting at 1)

# which columns of the entity model are not hard-coded?
varCols <- function(model)
{
	ncols <- model$getNColumns()
	cols <- numeric(0)
  n_hard_coded <- model$getData("n-hard-coded")
	if (n_hard_coded < ncols)
		cols <- (n_hard_coded+1):ncols
	cols
}
# which columns of the entity model are in 'user' space?
# meaning: not hard-coded and not entity lists
userCols <- function(model)
{
	cols <- varCols(model)
	cols[!(cols %in% entityListCols(model))]
}
# which columns of an entity model correspond to entity lists?
entityListCols <- function(model)
{
	which(colnames(model) %in% getListNames())
}
