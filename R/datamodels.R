######## Here are the functions for conveniently interacting with our custom GTK data models

# merging data models
mergeInfo <- function(old_info, info, def = NULL)
{
	info <- as.data.frame(info)
  
	# trim white space
	info[,1] <- trimWhiteSpace(as.character(info[,1]))
  
	colnames(info)[1] <- "ID" # by convention, first column is unique id
	
  #if (nrow(old_info) == 0) { # this lets us use merge()
	#	old_info <- rbind(old_info, rep(NA, ncol(old_info)))
	#} 
	
  match_ids <- match(info[,"ID"], old_info[,"ID"])
  not_matches <- is.na(match_ids)
  rev_matches <- match(old_info[,"ID"], info[,"ID"])
  col_intersect <- colnames(info) %in% colnames(old_info)
  new_cols <- which(!col_intersect)
  common_cols <- colnames(info)[col_intersect][-1]
  common_rows <- !is.na(rev_matches)
  if (!all(not_matches) && length(common_cols) > 0)
    # FIXME: tends to have problems with factors (conflicting levels)
    old_info[common_rows,common_cols] <- info[rev_matches[common_rows], common_cols]
  if (length(new_cols) > 0) {
    cols <- info[rev_matches,new_cols,drop=F]
    #merged_info <- cbind(old_info, cols)
    old_info$appendColumns(cols)
  } #else merged_info <- old_info
  if (any(not_matches)) {
    rows <- as.data.frame(matrix(NA, nrow=length(which(not_matches)), ncol=ncol(old_info)))
    colnames(rows) <- colnames(old_info)
    if (!is.null(def))
      rows[,names(def)] <- as.data.frame(def, stringsAsFactors = FALSE)
    rows[,colnames(info)] <- info[not_matches,]
    old_info$appendRows(rows)
    # set global filter to TRUE
    #old_info[is.na(old_info[,".visible"]),".visible"] <- TRUE
  }
	
	# Get rid of any garbage (including possible dummy row from above)
	#id_na <- which(is.na(merged_info[,"ID"]))
	#if (length(id_na) > 0)
	#	merged_info <- merged_info[-id_na,]
	
	old_info # success
}

#################### Data model construction ####################

createDataModel <- 
function(columns, d = NULL)
{
  model <- rGtkDataFrame(empty_data_frame(columns))
  model$setData("n-hard-coded", length(columns))
  if (!is.null(d))
    model$appendRows(d)
  model
}
