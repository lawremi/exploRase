##################### Simple Subset Dialog #####################

subsetDialog <- function(ent_type = exp_entityType(), parent = getMainWindow()) {
  subsetWindow <- gtkWindow("toplevel", show = F)
  subsetWindow$setTitle(paste("Subset", getEntityTypes()[ent_type]))
  subsetWindow$setTransientFor(parent)
  subsetWindow$setDestroyWithParent(T)
  subsetWindow$setDefaultSize(200,100)
  
  vbox <- gtkVBox(F, 3)
  subsetWindow$add(vbox)
  
  subsetFrame <- gtkFrame("Subset the data by the selected criteria")
  subsetFrame$setBorderWidth(3)
  
  vbox$packStart(subsetFrame, T, T, 0)
  
  filters <- list(
    "All values should be greater than" = 
      c(Column="(min value)", Relation=">"),
    "At least one fold change should be greater than" = 
      c(Column="(max fold change)", Relation=">"),
    "Variance between replicates should be less than" = 
      c(Column="(max replicate variance)", Relation="<"))
  
  subset_funcs <- c(calcMinValue, calcMaxFoldChange, calcMinRepVar)
  
  filtersTable <- gtkTable(length(filters), 3)
  filtersTable$setBorderWidth(3)
  filtersTable$setColSpacings(5)
  subsetFrame$add(filtersTable)
  
  filterEntries <- filterButtons <- list()
  
  sapply(1:length(filters), function(i) {
    filterButton <- gtkCheckButton(names(filters)[i])
    filterButtons <<- c(filterButtons, filterButton)
    filtersTable$attachDefaults(filterButton, 0, 1, i-1, i)
    filterEntry <- gtkEntry()
    filterEntries <<- c(filterEntries, filterEntry)
    filterEntry$setSizeRequest(50, -1)
    filtersTable$attachDefaults(filterEntry, 1, 2, i-1, i)
    filterSliderButton <- gtkButton("Show slider")
    filtersTable$attachDefaults(filterSliderButton, 2, 3, i-1, i)
    gSignalConnect(filterSliderButton, "clicked", showSubsetSlider_cb, 
      list(ent_type = ent_type, func = subset_funcs[[i]], entry = filterEntry, 
        relation = filters[[i]]["Relation"], row = i))
  })
  
  buttons <- gtkHButtonBox()
  buttons$setLayout("end")
  applyButton <- gtkButtonNewFromStock("gtk-apply")
  gSignalConnect(applyButton, "clicked", applySubset_cb, 
    list(ent_type = ent_type, filters = filters, entries = filterEntries, 
      buttons = filterButtons))
  buttons$add(applyButton)
  vbox$packStart(buttons, F, F, 0)
  vbox$setBorderWidth(3)
  
  subsetWindow
}

applySubset_cb <- function(button, user_data)
{
  ent_type <- user_data$ent_type
  model <- getFilterModel(ent_type)
  exp_removeFilterRules(model[,"ID"][grep("^\\(subset\\)", model[,"ID"])], ent_type)
  active <- sapply(user_data$buttons, gtkToggleButtonGetActive)
  if (any(active)) sapply((1:length(user_data$filters))[active], 
    function(i) addFilterRule(as.list(c(ID = paste("(subset)", i), 
      user_data$filters[[i]], Expression = user_data$entries[[i]]$getText()), ent_type)))
}

showSubsetSlider_cb <- function(button, user_data)
{
  values <- user_data$func(user_data$ent_type)
  filterScale <- gtkHScale(, 1, 100, 1)
  filterScale$setSizeRequest(100, -1)
  filtersTable <- button$getParent()
  filtersTable$remove(button)
  filtersTable$attachDefaults(filterScale, 2, 3, user_data$row-1, user_data$row)
  sorted_values <- sort(values, decreasing = user_data$relation == ">")
  percentiles <- format(sorted_values[floor((1:100)/100 * length(values))], digits = 3)
  gSignalConnect(filterScale, "value-changed", subsetScaleChanged_cb, 
    list(entry = user_data$entry, percentiles = percentiles))
  gSignalConnect(filterScale, "format-value", subsetScaleFormat_cb, values)
  gSignalConnect(user_data$entry, "activate", subsetEntryActivated_cb, 
    list(scale = filterScale, values = sorted_values))
  filterScale$setValue(50)
}

subsetScaleFormat_cb <- function(scale, percent, values)
{
  paste(percent, "% (", floor(percent/100 * length(values)), ")", sep = "")
}

subsetScaleChanged_cb <- function(scale, user_data)
{
  user_data$entry$setText(user_data$percentiles[scale$getValue()])
}

# FIXME: Does not work yet (attempt to set invalid type)
subsetEntryActivated_cb <- function(entry, user_data)
{
  # find the closest index
  diffs <- abs(user_data$values - as.numeric(entry$getText()))
  closest <- which.min(diffs)
  # shift up if not exactly equal
  closest <- closest + as.numeric(diffs[closest] != 0)
  user_data$scale$setValue(closest / length(user_data$values))
}

##################### Subsetting functions ##############

calcMinValue <- function(ent_type) {
  printTask("Finding min values")
  results <- apply(exp_dataset(ent_type), 1, min)
  finishTask()
  results
}
calcMaxFoldChange <- function(ent_type) {
  printTask("Finding fold changes")
  data_set <- exp_dataset(ent_type)
  results <- abs(apply(data_set, 1, max) - apply(data_set, 1, min))
  finishTask()
  results
}
calcMinRepVar <- function(ent_type) {
  printTask("Finding min rep variance")
  results <- apply(applyByReps(var, ent_type = ent_type), 1, min)
  finishTask()
  results
}

##################### Filter GUI ########################

filterView <- function(ent_type) {
  model <- getFilterModel(ent_type)
  entityModel <- getEntityModel(ent_type)
  filterBox <- gtkVBox(F, 2)
  filterBox$setBorderWidth(5)
  #filterBox$setData("entity-model", entityModel)
  
  #gSignalConnect(model, "row-inserted", filterRowInserted_cb, ent_type)
  #gSignalConnect(model, "row-deleted", filterRowDeleted_cb, ent_type)
  col_names <- colnames(model)
  filterTreeView <- gtkTreeView(model)
  filterTreeView$setSizeRequest(-1, 80)
  filterTreeView$getSelection()$setMode("multiple")
  textColumnsToView(filterTreeView, col_names[2:(ncol(model)-2)])
  filterTreeView$insertColumnWithAttributes(-1, "Expression", gtkCellRendererText(),
    text = match("Expression", col_names)-1, 
    background = match(".color", col_names)-1)
  toggleRenderer <- gtkCellRendererToggle()
  gSignalConnect(toggleRenderer, "toggled", activateToggle_cb, ent_type)
  filterTreeView$insertColumnWithAttributes(0, "Active", toggleRenderer,
    active = match("Active", col_names)-1)
  
  ruleBox <- gtkHBox(F, 3)
  filterBox$packStart(ruleBox, T, F, 0)
  buttons <- gtkHBox(F, 3)
  filterBox$packStart(buttons, T, F, 0)
  filterBox$packStart(scrollView(filterTreeView), T, F, 0)
  
  # fix me - how to keep the column combo box synchronized?
  columnModel <- rGtkDataFrame(getFilterColumns(entityModel))
  columnBox <- gtkComboBox(columnModel)
  model$setData("columnBox", columnBox)
  renderer <- gtkCellRendererText()
  columnBox$packStart(renderer)
  columnBox$setAttributes(renderer, "text" = 0)
  columnBox$setRowSeparatorFunc(columnBoxRowSeparator_func)
  ruleBox$packStart(columnBox, F, F, 0)
  
  opBox <- gtkComboBox(rGtkDataFrame(empty_data_frame("character")))
  renderer <- gtkCellRendererText()
  opBox$packStart(renderer)
  opBox$setAttributes(renderer, "text" = 0)
  ruleBox$packStart(opBox, F, F, 0)
  
  gSignalConnect(columnBox, "changed", filterColumnChanged_cb, ent_type)
  columnBox$setActive(0) # this sets everything up
  
  applyButton <- gtkButtonNewFromStock("gtk-apply")
  gSignalConnect(applyButton, "clicked", applyFilterRule_cb, ruleBox)
  buttons$packStart(applyButton, F, F, 0)
  buttons$packStart(gtkVSeparator(), F, F, 0)
  
  saveButton <- gtkButtonNewFromStock("gtk-save")
  gSignalConnect(saveButton, "clicked", saveFilterRule_cb, ruleBox)
  buttons$packStart(saveButton, F, F, 0)
  deleteButton <- gtkButtonNewFromStock("gtk-delete")
  gSignalConnect(deleteButton, "clicked", deleteFilterRule_cb, filterTreeView)
  buttons$packStart(deleteButton, F, F, 0)
  buttons$packStart(gtkVSeparator(), F, F, 0)
  
  deactivateAllButton <- gtkButtonNewFromStock("gtk-clear")
  buttons$packStart(deactivateAllButton, F, F, 0)
  gSignalConnect(deactivateAllButton, "clicked", deactivateAllRules_cb, ent_type)
  activateAllButton <- gtkButtonNewFromStock("gtk-select-all")
  buttons$packStart(activateAllButton, F, F, 0)
  gSignalConnect(activateAllButton, "clicked", activateAllRules_cb, ent_type)
  
  filterBox
}

filterRuleFromGUI <- function(ruleBox)
{
  columnBox <- ruleBox$getChildren()[[1]]
  opBox <- ruleBox$getChildren()[[2]]
  exprWidget <- NULL
  filter_expr <- filter_color <- NULL
  filter_col <- as.character(columnBox$getModel()[columnBox$getActive()+1,1])
  filter_op <- as.character(opBox$getModel()[opBox$getActive()+1,1])
  if (length(ruleBox$getChildren()) == 3) {
    exprWidget <- ruleBox$getChildren()[[3]]
    if (inherits(exprWidget, "GtkEntry"))
      filter_expr <- exprWidget$getText()
    else {
      filter_expr <- as.character(exprWidget$getModel()[exprWidget$getActive()+1,1])
      if (filter_col == "color")
        filter_color <- as.character(exprWidget$getModel()[exprWidget$getActive()+1,2])
    }
  }
  list(Column = filter_col, Relation = filter_op, Expression = filter_expr, .color = filter_color)
}

# both the operators and expression widget depend on the column
filterColumnChanged_cb <- function(columnBox, ent_type)
{
  # first get everything we need from the filter
  ruleBox <- columnBox$getParent()
  opBox <- ruleBox$getChildren()[[2]]
  exprWidget <- NULL
  if (length(ruleBox$getChildren()) == 3)
    exprWidget <- ruleBox$getChildren()[[3]]
  filter_col <- as.character(columnBox$getModel()[columnBox$getActive()+1,1])
  entityModel <- getEntityModel(ent_type)
  
  filterOpsAreDiscrete <- function(ops) ops[1] == "is" & ops[2] == "is not"
  
  # do we need to switch expression widget types?
  ops <- getFilterOps(filter_col, entityModel)
  old_ops <- opBox$getModel()[,1]  
  needNewWidget <- F
  if (!(length(ops) == length(old_ops) && all(ops == old_ops))) {
    opBox$setModel(rGtkDataFrame(data.frame(ops)))
    opBox$setActive(0)
     if (filterOpsAreDiscrete(ops) || filterOpsAreDiscrete(old_ops))
      needNewWidget <- T # switch between GtkComboBox and GtkEntry
  }
  
  # do we need to switch from color selection to boolean selection?
  if (!needNewWidget && inherits(exprWidget, "GtkComboBox")) {
    exprs <- getFilterExprs(filter_col, entityModel)
    old_exprs <- exprWidget$getModel()[,1]
    if (!(length(exprs[,1]) == length(old_exprs) && all(exprs[,1] == old_exprs)))
      needNewWidget <- T # we don't have to use a new widget here, but it simplifies code
  }
  
  if (needNewWidget) {
    if (filterOpsAreDiscrete(ops)) { # we now need a combo box for expression
      newExprWidget <- gtkComboBox(rGtkDataFrame(getFilterExprs(filter_col, entityModel)))
      renderer <- gtkCellRendererText()
      newExprWidget$packStart(renderer)
      newExprWidget$setAttributes(renderer, "text" = 0)
      if (filter_col == "color") # only set background for color
        newExprWidget$addAttribute(renderer, "background", 1)
      newExprWidget$setActive(0)
    } else if (filterOpsAreDiscrete(old_ops)) # now we need just an entry
      newExprWidget <- gtkEntry()
    if (!is.null(exprWidget)) # replace old widget with new one
      exprWidget$destroy()
    ruleBox$packStart(newExprWidget, F, F, 3)
  }
}

.quickFilterRule <- "(quick filter)"

applyFilterRule_cb <- function(button, ruleBox)
{
  addFilterRule(c(ID = .quickFilterRule, filterRuleFromGUI(ruleBox)))
}
saveFilterRule_cb <- function(button, ruleBox)
{
  addFilterRule(filterRuleFromGUI(ruleBox))
}
deleteFilterRule_cb <- function(button, filterTreeView)
{
  exp_removeFilterRules(getSelectedData(filterTreeView, "ID"))
}

activateToggle_cb <- function(renderer, path, ent_type)
{
  filterModel <- getFilterModel(ent_type)
  rule <- filterModel[as.numeric(path)+1,"ID"]
  exp_toggleFilterRules(rule, !renderer$getActive(), ent_type)
}
deactivateAllRules_cb <- function(button, ent_type)
{
  filterModel <- getFilterModel(ent_type)
  exp_toggleFilterRules(filterModel[filterModel[,"Active"],"ID"], F, ent_type)
}
activateAllRules_cb <- function(button, ent_type)
{
  filterModel <- getFilterModel(ent_type)
  exp_toggleFilterRules(filterModel[!filterModel[,"Active"],"ID"], T, ent_type)
}

getFilterColumns <- function(model) {
  cols <- colnames(model)
  filterCols <- c("color", "ID")
  user <- cols[userCols(model)]
  user <- user[grep("^[^.]", user)]
  if (length(user) > 0)
    filterCols <- c(filterCols, "", user)
  entityList <- cols[entityListCols(model)]
  if (length(entityList) > 0)
    filterCols <- c(filterCols, "", entityList)
  filterCols <- c(filterCols, "", "S expression")
  data.frame(cols=filterCols)
}
getFilterOps <- function(filter_col, model) {
  if (filter_col == "S expression")
    "satisfies"
  else {
    v <- model[,filter_col]
    if (filter_col == "color" || is.logical(v))
      c("is", "is not")
    else if (is.factor(v) || is.character(v))
      c("contains", "lacks", "matches", "starts with", "ends with", "regex")
    else c("<", ">", "<=", ">=", "==", "!=")
  }
}
getFilterExprs <- function(filter_col, model) {
  if (filter_col == "color")
    data.frame("names"=names(getGGobiColors()), "colors"=unlist(getGGobiColors()))
  else if (filter_col != "S expression" && is.logical(model[,filter_col])) 
    data.frame(values = c("TRUE", "FALSE")) # only TRUE/FALSE for logical
  else # otherwise, let them enter their own
    empty_data_frame("character")
}

updateFilterColumnBox <- function(ent_type) {
  columnBox <- getFilterModel(ent_type)$getData("columnBox")
  active <- columnBox$getActive()
  columnBox$setModel(rGtkDataFrame(getFilterColumns(getEntityModel(ent_type))))
  columnBox$setActive(active)
}

#################### Filter model #######################

getFilterModels <- function() .exp$getFilterModels()
getFilterModel <- function(ent_type = exp_entityType()) getFilterModels()[[ent_type]]

filterModel <- function() {
  cols <- c("Active" = "logical", "ID" = "character", "Column" = "character", 
    "Relation" = "character", "Expression" = "character", ".color" = "character")
  rGtkDataFrame(empty_data_frame(cols))
}

# Add a filter rule
# Adds a filter rule to the exploRase filter model for the given entity type
# @arguments A unique identifier for the filter rule
# @arguments The metadata column checked by the rule
# @arguments The operator (<, >, ==, etc) used for checking the values
# @arguments The right hand expression against which the values are checked
# @arguments Whether the rule should be immediately active
# @arguments The entity type of the metadata being filtered
# @keyword manip
exp_addFilterRule <- function(id, column, op, expr, active = T, ent_type = exp_entityType())
{
  rule <- list(ID = id, Column = column, Relation = op, Expression = expr, Active = active)    
  addFilterRule(rule, ent_type)
}

genUniqueFilterId <- function(model) 
{
  i <- as.character(1:nrow(model))
  i[!(i %in% model[,"ID"])][1]
}

addFilterRule <- function(rule, ent_type = exp_entityType()) 
{
  model <- getFilterModel(ent_type)
  
  # only the column, op, and expr is required
  if (is.null(rule$ID))
    rule["ID"] <- genUniqueFilterId(model)
  if (is.null(rule$Active))
    rule["Active"] <- T
  if (is.null(rule$.color))
    rule[".color"] <- "#FFFFFF"
  
  # sort the rule fields
  rule <- rule[colnames(model)]
  
  # if rule with that ID already exists, replace it
  id_match <- match(rule["ID"], model[,"ID"])
  if (!is.na(id_match)) {
    model[id_match,] <- rule
    refreshFilter(ent_type) # to remove the old one
  } else {
    model$appendRows(as.data.frame(rule, stringsAsFactors=FALSE))
    mergeFilterRules(model[nrow(model),,drop=F], ent_type)
  }
}

# Remove filter rules
# Removes the filter rules with the given identifiers for the given type
# @arguments the identifiers of the rules to remove
# @arguments the entity type of the metadata being filtered
# @keyword manip
exp_removeFilterRules <- function(rules, ent_type = exp_entityType())
{
  model <- getFilterModel(ent_type)
  model$setFrame(model[!(model[,"ID"] %in% rules),,drop=F])
  # right now this just reprocesses all of the rules minus the unmerged one
  # this is not that efficient, but it avoids having to store results
  refreshFilter(ent_type)
}

# Get filter rules
# Gets the filter rules for the metadata of the given type
# @arguments the entity type of the metadata being filtered by the rules
# @keyword manip
exp_filterRules <- function(ent_type = exp_entityType())
{
  getFilterModel(ent_type)[,"ID"]
}

# Toggle filter rules
# Toggles (activates or deactivates) the specified rules for the specified type.
# @arguments The identifiers of the rules to toggle
# @arguments Whether to activate or deactivate the rules
# @arguments The entity type of the metadata being filtered by the rules
# @keyword manip
exp_toggleFilterRules <- function(rules, active = T, ent_type = exp_entityType())
{
  model <- getFilterModel(ent_type)
  which_rules <- model[,"ID"] %in% rules
  model[which_rules, "Active"] <- active
  if (active)
    mergeFilterRules(model[which_rules,,drop=F], ent_type)
  else refreshFilter(ent_type)
}

#################### Low level filter functions ###################

mergeFilterRules <- function(rules, ent_type, clear = F)
{
  ent_model <- getEntityModel(ent_type)
  if (clear)
    result <- rep(T, nrow(ent_model))
  else result <- ent_model[,1]
  filter_model <- getFilterModel(ent_type)
  active <- filter_model[filter_model[,"ID"] == rules[,"ID"], "Active"]
  if (any(active))
    sapply((1:nrow(rules))[active], function(i)
      result <<- result & processFilterRule(rules[i,], ent_type))
  return(applyFilter(result, ent_type))
}

refreshFilter <- function(ent_type)
{
  return(mergeFilterRules(getFilterModel(ent_type), ent_type, T))
}

processFilterRule <- function(rule, ent_type)
{
  ent_model <- getEntityModel(ent_type)
  cols <- colnames(ent_model)
  column <- rule$Column
  op <- rule$Relation
  expr <- rule$Expression
  if (column %in% colnames(ent_model)) {
    column_data <- ent_model[,column]
    env <- list(column_data = column_data)
  } else
    ent_match <- match(getEntityIds(ent_type), rownames(exp_dataset(ent_type)))
  
  result <- rep(F,nrow(ent_model))
  
  if (column == "(min value)")
    result <- calcMinValue(ent_type)[ent_match] > as.numeric(expr)
  else if (column == "(max fold change)")
    result <- calcMaxFoldChange(ent_type)[ent_match] > as.numeric(expr)
  else if (column == "(max replicate variance)") {
    result <- calcMinRepVar(ent_type)[ent_match] < as.numeric(expr)
  } else if (op == "is" || op == "is not" || op == "matches") {
    if (column == "color")
      expr <- getGGobiColors()[expr]
    result <- column_data == expr
    if (op == "is not")
      result <- !result
  } else if (op == "satisfies")
    result <- eval(parse(text=expr), as.data.frame(ent_model))
  else if (is.factor(column_data) || is.character(column_data)) {
    column_data <- as.character(column_data)
    fixed <- F
    if (op == "contains" || op == "lacks")
      fixed <- T
    else if (op == "starts with")
      expr <- paste("^", expr, sep="")
    else if (op == "ends with")
      expr <- paste(expr, "$", sep="")
    result[grep(expr, column_data, fixed = fixed)] <- T
    if (op == "lacks")
      result <- !result
  } else {
    # need to check for a numeric value here
    result <- eval(parse(text=paste("column_data", op, expr)), env)
  }
  
  return(result)
}

applyFilter <- function(result, ent_type)
{
  entModel <- getEntityModel(ent_type)
  if (!any(entModel[,1] != result))
    return(F)
  entModel[,1] <- result
  entView <- getEntityView(ent_type)
  entView$setModel(rGtkDataFrame(entModel[result,,drop=F]))
  dataset <- .exp$getGobisets()[[ent_type]]
  if (!is.null(dataset))
    excluded(dataset)[match(as.character(entModel[,"ID"]),
      rownames(dataset))] <- !result
  return(T)
}

