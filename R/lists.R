
##################### The list view ######################

# selects the entities in the list
# 'name' is the name of the entity list
highlightList <- function(list_name) {
  sapply(exp_entityTypes(), function(ent_type) {
    model <- getEntityModel(ent_type)
    name_ind <- match(list_name, colnames(model))[1]
		if (!is.na(name_ind))
			highlightEntities(model[,list_name], ent_type)
	})
}

listNameEdited_cb <- function(cell, path.string, new.text, model)
{
  ind <- as.numeric(path.string)+1
  models <- getEntityModels()
  prev_name <- model[ind,"name"]
  model[ind,"name"] <- new.text
  sapply(exp_entityTypes(), function(ent_type) {
    ent_model <- models[[ent_type]]
    colnames(ent_model)[match(prev_name, colnames(ent_model))] <- new.text
    rows <- updateModelListColumn(ent_model)
    propagateEntityInfo(ent_type, rows)
    updateFilterColumnBox(ent_type)
  })
}

# here we just get the selected entity list and "highlight" its ents
listSelectRow_cb <- function(treeselection, user.data)
{
	entity_list <- exp_listSelection()

	if (length(entity_list) > 0) 		# now highlight the list
		highlightList(entity_list[[1]])
}

listDoubleClick_cb <- function(listView, event, user_data)
{
  if (event[["type"]] == "2button-press") {
    listFilterNames <- paste("(", getListNames(), ")", sep="")
    listName <- exp_listSelection()
    sapply(exp_entityTypes(), function(ent_type) {
      activeListFilters <- listFilterNames %in% exp_filterRules(ent_type)
      if (any(activeListFilters))
        exp_removeFilterRules(listFilterNames[activeListFilters])
      exp_addFilterRule(paste("(", listName, ")", sep=""), listName, "is", "TRUE",
        ent_type = ent_type)
    })
  }
}

listView <- function(listModel)
{
	listView <- gtkTreeView(listModel)
  col_names <- "name"
  textColumnsToView(listView, col_names, editors = list(listNameEdited_cb))
  #gSignalConnect(listView, "button-press-event", listDoubleClick_cb)
	listView
}

getListView <- function() .exp$getListView()

# Get list selection
# Gets the selected lists in the exploRase GUI
# @value Names of selected lists
# @keyword manip
exp_listSelection <- function() getSelectedData(getListView(), "name")

########################## List model #######################

# creates the simple gene list model, which has one column for the list names
# if 'entity_lists' is provided, this is loaded into the data frame
listModel <- 
function(entity_lists = NULL)
{
  cols <- c("name" = "character")
  createDataModel(cols, entity_lists)
}

createList_cb <- function(w, u = NULL)
{
  l <- createList(paste("List", getNLists()))
  assert(length(l) > 0, "Please select some entities")
  exp_loadLists(list(l))
  listView <- getListView()
  listView$grabFocus()
  path <- gtkTreePathNewFromIndices(nrow(listView$getModel())-1)
  listView$scrollToCell(path)
  listView$setCursor(path, listView$getColumns()[[1]], T)
}

# FIXME: Handle the case of gene lists with same name

# Load entity lists
# Loads a list of entity lists (matrices) into exploRase
#
# An entity list matrix may have one or two columns. The last column specifies
# the entity ID's and its name is the name of the entity list. 
# If there are two columns, the first specifies the type
# of each entity, allowing entity lists holding entities of different types.
# 
# @arguments a list of entity lists (1 or 2 column matrices)
# @keyword manip
exp_loadLists <- function(ent_lists) {
	list_names <- NULL
	ent_cols <- NULL
  tmp_ent_lists <- list()
	for (ent_list in ent_lists) {
		assert(length(dim(ent_list)) == 2 && dim(ent_list)[2] > 0, 
		  "Entity lists must be bidimensional")
    if (ncol(ent_list) < 2)
      ent_list <- cbind("gene", ent_list)
    ent_list[,2] <- trimWhiteSpace(ent_list[,2])
		addEntities(ent_list[,2], ent_list[,1])  # discover any new entities
    tmp_ent_lists <- c(tmp_ent_lists, list(ent_list))
	}
  names(tmp_ent_lists) <- names(ent_lists)
  ent_lists <- tmp_ent_lists
	ent_ids <- getEntityIds()
	for (ent_list in ent_lists) {
		assert(length(colnames(ent_list)) > 0, "Entity lists must have names")
		list_name <- colnames(ent_list)[2]
		list_names <- rbind(list_names, data.frame(name=I(list_name)))
		# add gene list filter columns
		ent_cols <- rbind(ent_cols, ent_ids %in% ent_list[,2])
	}
	if (length(ent_lists) > 0) {
		getListModel()$appendRows(list_names)
		rownames(ent_cols) <- t(list_names)
		addInfoColumns(t(ent_cols))
    #updateListColumn()
	}
  projectStarted()
}

# Create an entity list
# Forms the actual entity list matrix from the name, ents, and types
# @arguments The name of the entity list
# @arguments The ID's of the entities in the list
# @arguments The types of the entities in the list
# @value A matrix conforming to the structure for entity lists
# @keyword manip
exp_newList <- function(name, ents, types)
{
  assert(length(name) > 0 && nchar(name) > 0)
  if (length(ents) == 0)
    return(NULL)
  ent_list <- data.frame(.type = types, ents)
  colnames(ent_list)[2] <- name
  ent_list
}

# gets the matrix representation of an existing entity list given the name
getListMatrix <- function(name) {
  models <- getEntityModels()
  ent_list <- NULL
  sapply(exp_entityTypes(), function(type) ent_list <<- rbind(ent_list,
    exp_newList(name, exp_entitiesInList(name, type), type)))
  ent_list
}

# creates an entity list from selections in the entity views
createList <- function(name)
{
  ent_list <- NULL
  sapply(exp_entityTypes(), function(type) ent_list <<- rbind(ent_list, 
    exp_newList(name, exp_entitySelection(type), type)))
	ent_list
}

getNLists <- function() {
	length(getListNames())
}
getListNames <- function() {
	as.character(exp_listFrame()[,"name"])
}
getListModel <- function() .exp$getListModel()

# Get entity lists
# Gets the entity lists loaded in exploRase
# @value A data frame with a single column "name" holding the names of the lists
# @keyword manip
exp_listFrame <- function() as.data.frame(getListModel())

# FIXME: shouldn't this return a list matrix?

# Get the entities in a list
# Gets the ID's of the entities in a specified list and of the specified types.
# This is useful if you only have to list name and not the matrix.
# @arguments The name of the entity list
# @arguments The types of entities to return
# @value The entity ID's of the specified types belonging to the list
# @keyword manip
exp_entitiesInList <- function(list, types = exp_entityTypes()) {
	models <- getEntityModels()
	unlist(sapply(types, function(type) {
		entities <- as.data.frame(models[[type]])
		col <- match(list, colnames(entities))
		getEntityIds(type)[entities[,col]]
	}))
}

