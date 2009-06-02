# General GUI functionality and utilities


########################## The core GUI #########################

getMainWindow <- function() .exp$getMainWindow()

MnuExit_cb <- function(w, event, should_quit = F)
{
   exp_close()
   if (should_quit)
     quit("no")
}

####################### GtkTreeView utilities ########################

# gets the values in cols for selected rows in a TreeView
getSelectedData <-
  function(treeView, cols = seq_len(treeView$getModel()$getNColumns()))
{
  model <- treeView$getModel()
  treesel <- treeView$getSelection()
	rows <- treesel$getSelectedRows()$retval
	if (length(rows) > 0) {
		model[rows, cols]
  } else NULL
}

# adds a set of text columns to 'view' with 'colnames' using 'cols' from the view's model
# the columns are inserted at the position specified by 'insert'
# if 'editors' are specified, the columns are editable and 'edit_starters' if specified
# are called when editing begins while 'editors' are called after editing has finished.
# The user data for the callbacks is given as 'data'
# If 'combo' is TRUE, combobox renderers are used instead of simple text renderers
textColumnsToView <- function(treeView, col_names, 
    cols=match(col_names, colnames(treeView$getModel()))-1, insert = -1, 
    width = rep(120, length(col_names)), editors, edit_starters, 
    user_data = treeView$getModel(), combo = F) {
  if (length(col_names) == 0)
    return()
  columns <- NULL
	for (i in 1:length(col_names)) {
    if (combo) {
      renderer <- gtkCellRendererCombo()
      renderer$set(model = rGtkDataFrame(empty_data_frame("character")), 
        text_column = 0, has_entry = F)
    } else renderer <- gtkCellRendererText()
    renderer$setData("model", treeView$getModel())
    if (!missing(editors) && !is.null(editors[[i]])) {
      renderer$set(editable = T)
      gSignalConnect(renderer, "edited", editors[[i]], user_data)
      if (!missing(edit_starters) && !is.null(edit_starters[[i]]))
        gSignalConnect(renderer, "editing-started", edit_starters[[i]], user_data)
    }
	  column <- gtkTreeViewColumn(col_names[i], renderer, text = cols[i])
    column$setSizing("fixed")
    column$setFixedWidth(width[i])
	  treeView$insertColumn(column, insert)
	  columns <- c(columns, column)
  }
	columns
}

rootModel <- function(treeView) {
  model <- treeView$getModel()
  while(!inherits(model, "RGtkDataFrame")) 
    model <- model$getModel()
  model
}

# this configures all columns for 'view' using 'cols' as the sort id's. 
configureViewColumns <- function(treeView, cols)
{
	columns <- treeView$getColumns()
	if (missing(cols)) {
    titles <- sapply(columns, gtkTreeViewColumnGetTitle)
		cols <- match(titles, colnames(treeView$getModel()))
  }
	for (i in 1:length(cols)) {
	  configureViewColumn(columns[[i]], cols[i])
	}
}

# configures a view column so that it is reorderable, resizable, and
# sorted on 'sort_col' as long as 'sort_col' != -1
# if 'removable' is TRUE, this column is considered temporary
# 'keyword' will link columns to the context-sensitive help
configureViewColumn <- function(column, sort_col = -1, removable = T, 
  desc = column$getTitle(), keyword = desc) {
  
  column$setReorderable(TRUE)
	column$setResizable(TRUE)
  
  column$setData("keyword", keyword)
  column$setData("removable", removable)

  # serious GtkTreeView wizardry follows
  label <- gtkLabel(column$getTitle())
  label$setEllipsize("end")
  label["xalign"] <- 0
  column$setWidget(label)
  alignment <- label$getParent()
  alignment["xscale"] <- 1
  gSignalConnect(alignment, "notify::xscale", function(alignment, pspec)
  {
    if (alignment["xscale"] != 1)
      alignment["xscale"] <- 1
  })
  button <- alignment$getParent()$getParent()
  .exp$getTooltips()$setTip(button, desc, keyword)
  # wizardry concluded
  
  if (sort_col != -1) {
    column$setSortColumnId(sort_col-1)
  }
}

scrollView <- function(treeView)
{
   scroll <- gtkScrolledWindow(NULL, NULL)
   scroll$setPolicy("automatic", "automatic")
   scroll$add(treeView)
   scroll
}

viewColumnWidth_cb <- function(column, pspec) {
	w <- column$getWidth()
  
  if (w > 150 && !(column$getSizing() == "fixed")) {
    w <- 150
    column$setSizing("fixed")
    column$setFixedWidth(w)
  }
}

######################## Showing Results ###########################

# Show analysis results
# Add a column of analysis results to the exploRase table and the GGobi dataset.
# 
# @arguments the data frame of results. To include results for multiple
# entity types at once, the result for each type should be concatenated in the
# same order as the \code{types} parameter.
# @arguments a root label for the result (like the type of analysis)
# @arguments other labels concatenated to the root (such as the conditions involved)
# @arguments the entity type(s) of the data from which this result was derived.
# @arguments a keyword identifying this result, for use in context-sensitive help
# @arguments whether to show the result in the explorase table
# @arguments whether to add the result to the GGobi dataset
# @keyword GUI
# @keyword manip
exp_showResults <- function(results, label, sublabels = "", types = exp_entityType(),
	keyword = NULL, explorase = T, ggobi = T)
{
  printOp("Showing results")
	results_name <- createVarName(sublabels, label)
	if (explorase) {
		addResultColumn(results, results_name, types, keyword)
	}
	if (ggobi) {
		addGGobiVariable(results, results_name, types)
	}
}

################### Dialog utilities ##########################

errorDialog <- function(..., parent = getMainWindow())
{
	dialog <- gtkMessageDialog(parent, "destroy-with-parent", "error", "close", ...)
  gSignalConnect(dialog, "response", gtkWidgetDestroy)
}
warningDialog <- function(..., parent = getMainWindow())
{
	dialog <- gtkMessageDialog(parent, "destroy-with-parent", "warning", "close", ...)
  gSignalConnect(dialog, "response", gtkWidgetDestroy)
}

###################### Misc utilities #######################

columnBoxRowSeparator_func <- function(model, iter, data)
{
  if (model$get(iter, 0) == "")
    return(T)
  return(F)
}

# get an action (GtkAction) in the user interface by name
getAction <- function(name)
{
  groups <- .exp$getUIManager()$getActionGroups()
  group_names <- sapply(groups, gtkActionGroupGetName)
  groups[[which(group_names == "exploRaseActions")]]$getAction(name)
}
