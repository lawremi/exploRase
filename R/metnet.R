#################### MetNet integration #####################

if (FALSE) {
exp_metnet_loadExpData <- function()
{
  require(rJava)
  export_obj <- .jcall("edu/iastate/metnet/hivemindService/ExploRaseExportObj", 
    "Ledu/iastate/metnet/hivemindService/ExploRaseExportObj;", "getInstance")
  n_cols <- export_obj$getExperimentCount()
  exp_data <- sapply(1:n_cols, function(col) export_obj$getExperimentData(col))
  exp_data <- as.data.frame(exp_data)
  rownames(exp_data) <- export_obj$getEntities()
  colnames(exp_data) <- export_obj$getExperiments()
  exp_loadExpData(exp_data)
}
}
