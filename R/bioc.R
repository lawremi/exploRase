# Facilities for interacting with Bioconductor

exp_loadExpressionSet <- function(exprset, type = exp_entityType()) {
  exp_loadDesign(phenoData(exprset), type)
  exp_loadInfo(featureData(exprset), type)
  exp_loadData(exprs(exprset), type)
}

exp_phenoData <- function(type = exp_entityType()) {
  design <- exp_designFrame()
  rownames(design) <- design$ID
  design$ID <- NULL
  as(design, "AnnotatedDataFrame")
}

exp_featureData <- function(type = exp_entityType()) {
  info <- exp_entityFrame()
  rownames(info) <- info$ID
  info$ID <- NULL
  as(info, "AnnotatedDataFrame")
}

exp_expressionSet <- function(type = exp_entityType()) {
  new("ExpressionSet", exprs = as.matrix(exp_dataFrame(type)),
      phenoData = exp_phenoData(type),
      featureData = exp_featureData(type))
}
