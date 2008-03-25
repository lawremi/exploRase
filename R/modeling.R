################# Modeling GUI frontends ###################

temporalWindow <- function(parent = getMainWindow())
{
  window <- gtkWindow("toplevel", show = F)
  window$setTitle("Temporal modeling")
  window$setTransientFor(parent)
  window$setDestroyWithParent(T)
  window$setDefaultSize(300,200)
  
  vbox <- gtkVBox(F, 3)
  window$add(vbox)
  hbox <- gtkHBox(F, 3)
  vbox$packStart(hbox, T, T, 0)
  
  factorNames <- exp_designFactors()
  assert("time" %in% factorNames, "You need a 'time' variable in your experiment design info")
  hiddenFactors <- c(".visible", "ID", "replicate", "time")
  factorNames <- factorNames[-which(factorNames %in% hiddenFactors)]
  numFactors <- length(factorNames)
  factors <- NULL
  factorAndModelBox <- gtkVBox(F, 3)
  hbox$packStart(factorAndModelBox, F, F, 0)
  if (numFactors > 0) {
    factorFrame <- gtkFrame("Factors x time")
    factorAndModelBox$packStart(factorFrame, T, T, 0)
    factorList <- gtkVBox(F, 2)
    factorList$setBorderWidth(3)
    factorFrame$add(factorList)
    if (numFactors > 1)
    factorNames <- c(factorNames, sapply(1:(numFactors-1), function(ind)
      paste(factorNames[ind], factorNames[(ind+1):numFactors], sep="*")))
    factors <- sapply(factorNames, gtkCheckButton)
    sapply(factors, function(factor) {
      factorList$packStart(factor, F, F, 0)
      gSignalConnect(factor, "toggled", modelFactor_cb, factors)
    })
  }
  
  modelFrame <- gtkFrame("Time Model")
  factorAndModelBox$packStart(modelFrame, F, F, 0)
  modelVBox <- gtkVBox(F, 2)
  modelVBox$setBorderWidth(3)
  modelFrame$add(modelVBox)
  degreeHBox <- gtkHBox(F, 5)
  modelVBox$packStart(degreeHBox, F, F, 0)
  degreeHBox$packStart(gtkLabel("Polynomial degree"), F, F, 0)
  degreeButton <- gtkSpinButtonNewWithRange(1, 5, 1)
  degreeHBox$packStart(degreeButton, F, F, 0)
  actualTimeButton <- gtkRadioButton(NULL, "Actual time")
  virtualTimeButton <- gtkRadioButton(list(actualTimeButton), "Virtual time")
  modelVBox$packStart(virtualTimeButton, F, F, 0)
  modelVBox$packStart(actualTimeButton, F, F, 0)
  
  outputFrame <- gtkFrame("Output")
  hbox$packStart(outputFrame, T, T, 0)
  outputList <- gtkVBox(F, 2)
  outputList$setBorderWidth(3)
  outputFrame$add(outputList)
  outputNames <- c("p-values", "F-statistic", "coefficients", "error")
  outputs <- sapply(outputNames, gtkCheckButton)
  sapply(outputs, function(output) {
    output$setActive(T)
    outputList$packStart(output, F, F, 0)
  })
  
  buttons <- gtkHButtonBox()
  buttons$setLayout("end")
  applyButton <- gtkButtonNewFromStock("gtk-apply")
  user_data <- list(factors = factors, outputs = outputs, 
    degree = degreeButton, virtual_time = virtualTimeButton)
  gSignalConnect(applyButton, "clicked", applyTemporal_cb, user_data)
  buttons$add(applyButton)
  vbox$setBorderWidth(5)
  vbox$packStart(buttons, F, F, 0)
  
  window
}

limmaWindow <- function(parent = getMainWindow())
{
  window <- gtkWindow("toplevel", show = F)
  window$setTitle("Linear modeling via Limma")
  window$setTransientFor(parent)
  window$setDestroyWithParent(T)
  window$setDefaultSize(300,200)
  
  vbox <- gtkVBox(F, 3)
  window$add(vbox)
  hbox <- gtkHBox(F, 3)
  vbox$packStart(hbox, T, T, 0)
  
  factorFrame <- gtkFrame("Factors")
  hbox$packStart(factorFrame, T, T, 0)
  factorList <- gtkVBox(F, 2)
  factorFrame$add(factorList)
  factorNames <- exp_designFactors()
  hiddenFactors <- c(".visible", "ID", "replicate")
  factorNames <- factorNames[!(factorNames %in% hiddenFactors)]
  numFactors <- length(factorNames)
  assert(numFactors > 0, "You have no factors, please load some experimental design information")
  if (numFactors > 1)
    factorNames <- c(factorNames, sapply(1:(numFactors-1), function(ind)
      paste(factorNames[ind], factorNames[(ind+1):numFactors], sep="*")))
  factors <- sapply(factorNames, gtkCheckButton)
  sapply(factors, function(factor) {
    factorList$packStart(factor, F, F, 0)
    gSignalConnect(factor, "toggled", modelFactor_cb, factors)
  })
  
  outputFrame <- gtkFrame("Output")
  hbox$packStart(outputFrame, T, T, 0)
  outputList <- gtkVBox(F, 2)
  outputFrame$add(outputList)
  outputOptions <- c("p-values" = TRUE, "cor-p-values" = FALSE,
                     "F-statistics" = TRUE, "coefficients" = FALSE,
                     "fitted" = FALSE)
  outputNames <- names(outputOptions)
  outputs <- sapply(outputNames, gtkCheckButton)
  sapply(outputs, outputList$packStart, F, F, 0)
  mapply(gtkToggleButtonSetActive, outputs, outputOptions)
  
  limmaAdvanced <- gtkVBox(F, 3)
  advContrastFrame <- gtkFrame("Time contrasts")
  limmaAdvanced$packStart(advContrastFrame, T, T, 0)
  advContrastBox <- gtkVBox(F, 2)
  advContrastFrame$add(advContrastBox)
  advContrasts <- sapply(c("linear", "quadratic"), gtkCheckButton)
  sapply(advContrasts, function(contrast) advContrastBox$packStart(contrast, F, F, 0))
  advPAdjustFrame <- gtkFrame("Method for p-value adjustment")
  limmaAdvanced$packStart(advPAdjustFrame, T, F, 0)
  advPAdjustCombo <- gtkComboBoxNewText()
  advPAdjustFrame$add(advPAdjustCombo)
  sapply(p.adjust.methods, function(method) advPAdjustCombo$appendText(method))
  advPAdjustCombo$setActive(which(p.adjust.methods == "fdr")-1)
  
  advanced <- gtkExpander("Advanced")
  advanced$add(limmaAdvanced)
  vbox$packStart(advanced, F, F, 0)
  
  buttons <- gtkHButtonBox()
  buttons$setLayout("end")
  applyButton <- gtkButtonNewFromStock("gtk-apply")
  user_data <- list(factors = factors, outputs = outputs, 
    contrasts = advContrasts, pAdjustment = advPAdjustCombo)
  gSignalConnect(applyButton, "clicked", applyLimma_cb, user_data)
  buttons$add(applyButton)
  vbox$setBorderWidth(5)
  vbox$packStart(buttons, F, F, 0)
  
  window
}

############### Analysis routines / callbacks ##################

modelFactor_cb <- function(wid, factors)
{
  if (wid$getActive())
    sapply(factors[getInteractingTerms(wid$getLabel(), names(factors))], 
      gtkToggleButtonSetActive, T)
  if (!wid$getActive())
    sapply(factors, function(factor) 
      if (wid$getLabel() %in% getInteractingTerms(factor$getLabel(), names(factors)))
        factor$setActive(F))
}


temporal_cb <- function(wid, user_data)
{
  temporalWindow()$showAll()
}
applyTemporal_cb <- function(wid, user_data)
{
  outputs <- sapply(user_data$outputs, gtkToggleButtonGetActive)
  factors <- c(names(user_data$factors)[sapply(user_data$factors, gtkToggleButtonGetActive)])
  trueFactors <- factors[factors %in% exp_designFactors()]
  dups <- duplicated(interaction(exp_designFrame()[,c(trueFactors, "time")]))
  assert(any(dups), "Your model is fully specified and thus pointless")
  
  # first make times
  degree <- user_data$degree$getValue()
  times <- "time"
  if (degree > 1)  
    times <- c(times, paste("I(", paste("time", 2:degree, sep="^"), ")", sep=""))
  if (length(factors) > 0) # now the interactions with times
    times <- c(times, sapply(times, function(time) paste(time, factors, sep="*")))
  # create formula
  formula <- paste(".value ~", paste(times, collapse=" + "))
  print(formula)
  model_terms <- terms(as.formula(formula))
  model_labels <- attr(model_terms, "term.labels")
  time_terms <- !(model_labels %in% factors)
  time_names <- sub("I\\(([^)]*)\\)", "\\1", model_labels[time_terms])
  #print(time_names)
  time_terms <- c(FALSE, time_terms) # for intercept
  d <- exp_dataset() # all samples.. for now
  design <- exp_designFrame()
  if ("replicate" %in% colnames(design)) { # drop out the means
    d <- d[,!is.na(design[,"replicate"])]
    design <- design[!is.na(design[,"replicate"]),]
  }
  # virtual time?
  if (user_data$virtual_time$getActive())
    design[,"time"] <- as.numeric(as.factor(design[,"time"]))
  printTask("Fitting time models")
  printOp("Fit 0 models")
  inc <- 95 / (nrow(d) / 1000)
  count <- 0
  results <- t(apply(d, 1, function(ent) {
    # fit the model and calculate some statistics
    fit <- lm(model_terms, cbind(.value = ent, design))
    # leave off non-time factors and intercept
    fit.summary <- summary(fit)
    p <- coef(fit.summary)[time_terms,4]
    coeffs <- fit$coefficients[time_terms]
    names(p) <- names(coeffs) <- time_names
    f <- fit.summary$fstatistic
    count <<- count + 1
    if (count %% 1000 == 0) {
      addProgress(inc)
      printOp("Fitted", count, "models")
    }
    c(coeffs, p.F.model = pf(f[[1]], f[[2]], f[[3]]), F.model = f[[1]], 
      error = sum(fit$residuals[time_terms]^2), p = p)
  }))
  if (outputs["error"])
    exp_showResults(results[,"error"], "error", keyword="temporal")
  if (outputs["F-statistic"]) {
    if (outputs["p-values"])
      exp_showResults(results[,"p.F.model"], "p.F.model", keyword="temporal")
    exp_showResults(results[,"F.model"], "F.model", keyword="temporal")
  }
  if (outputs["p-values"]) {
    p_values <- colnames(results)[(which(colnames(results) == "error")+1):ncol(results)]
    sapply(p_values, function(p_value) exp_showResults(results[,p_value], p_value, keyword="temporal"))
  }
  if (outputs["coefficients"]) {
    coeffs <- colnames(results)[1:(which(colnames(results) == "p.F.model")-1)]
    # FIXME: do we want to add the time coefficients onto the other ones?
    sapply(coeffs, function(coeff) exp_showResults(results[,coeff], coeff, keyword="temporal"))
  }
  finishTask()
}

limma_cb <- function(wid, user_data)
{
  limmaWindow()$showAll()
}

applyLimma_cb <- function(wid, user_data)
{
  outputs <- sapply(user_data$outputs, gtkToggleButtonGetActive)
  factors <- names(user_data$factors)[sapply(user_data$factors, gtkToggleButtonGetActive)]
  assert(length(factors) > 0, "Please select at least one factor")
  trueFactors <- factors[factors %in% exp_designFactors()]
  design <- exp_designFrame()
  dataset <- exp_dataset()
  selected_conds <- exp_designSelection()
  if ("replicate" %in% colnames(design)) {
    real <- !is.na(design[,"replicate"])
    dataset <- dataset[,real]
    design <- design[real,]
    selected_conds <- selected_conds[selected_conds %in% rownames(design)]
  }
  if (length(selected_conds) > 1) {
    design <- design[match(selected_conds,design[,"ID"]),]
    dataset <- dataset[,selected_conds]
  }
  ints <- interaction(design[,trueFactors])
  sorted_ints <- sort.list(ints)
  # make sure the interactions match up with the experimental data
  ints <- ints[match(colnames(dataset), design[,"ID"])]
  # make sure the design matrix matches up with the levels of the interactions
  design <- design[sorted_ints,]
  dups <- duplicated(ints[sorted_ints])
  assert(any(dups), 
    "Model over-specified, please select a different (larger) set of conditions")
  model_matrix <- model.matrix(~0+ints)[,table(ints) > 0]
  
  # across all samples for now
  
  printTask("Limma")
  printOp("Fitting linear models")
  fit<-lmFit(dataset, model_matrix)
  addProgress(50)
    
  treatments <- design[!dups, trueFactors, drop=F]
  contrasts <- sapply(user_data$contrasts, gtkToggleButtonGetActive)
  assert(!any(contrasts) || "time" %in% colnames(treatments),
    "You must have a 'time' variable in your experimental design to find time contrasts")
  
  n_contrasts  <- length(factors)
  if (contrasts["linear"])
    n_contrasts <- n_contrasts + 1
  if (contrasts["quadratic"])
    n_contrasts <- n_contrasts + 1
  
  inc <- floor(50 / n_contrasts)
  
  fitContrast <- function(contrast, treatment) {
    printOp("Fitting contrasts for ", treatment)
    con_fit <- contrasts.fit(fit, contrast)
    fit_ebayes <- eBayes(con_fit)
    if (outputs["p-values"])
      exp_showResults(fit_ebayes$F.p.value, "p", treatment, keyword="limma")
    if (outputs["cor-p-values"]) {
      p.cor <- p.adjust(fit_ebayes$F.p.value,
                        method = user_data$pAdjustment$getActiveText())
      exp_showResults(p.cor, "p.cor", treatment, keyword="limma")
    }
    if (outputs["F-statistics"])
      exp_showResults(fit_ebayes$F, "F", treatment, keyword="limma")
    if (outputs["coefficients"])
      #for(contr_name in colnames(coeffs)) 
      #  exp_showResults(coeffs[,contr_name], "coeff", contr_name, keyword="limma")
      exp_showResults(fit_ebayes$coefficients[,1], "coeff", treatment, keyword="limma")
    if (outputs["fitted"]) {
      fitted_vals <- fitted(fit_ebayes)
      for(sample in colnames(fitted_vals)) 
        exp_showResults(fitted_vals[,sample], "fitted", 
          paste(treatment, sample, sep="."), keyword="limma", explorase=FALSE)
    }
    addProgress(inc)
  }
  
  result <- NULL
  sapply(factors, function(treatment) {
    ints <- getInteractingTerms(treatment, names(user_data$factors))
    if (length(ints) > 0)
      contrast <- makeContrastCross(makeContrastTrt(treatments[,ints[1]], model_matrix),
        makeContrastTrt(treatments[,ints[2]], model_matrix), model_matrix)
    else contrast <- makeContrastTrt(treatments[,treatment],model_matrix)
    fitContrast(contrast, treatment)
  })
  
  
  if (contrasts["linear"])
    fitContrast(makeContrastLinear(treatments[,"time"]), paste("time", "linear", sep="="))
  if (contrasts["quadratic"])
    fitContrast(makeContrastQuadratic(treatments[,"time"]), paste("time", "quadratic", sep="="))

  finishTask()
}

############## Utilities ################

getInteractingTerms <- function(int, terms, sep = "\\*")
{
  int_terms <- unlist(strsplit(int, sep))
  if(length(int_terms) > 1 && all(int_terms %in% terms))
    return(int_terms)
  return(character(0))
}

####################################### Limma utilities by Eun Kyung Lee

## make contrast.matrix for treatment effect and interaction effect

# "trt" can be  one row from exp.info as a factor
#  "design" is a design matrix from model.matrix function
# makeContrastTrt function returns contrast matrix for trt

makeContrastTrt<-function(trt,design)
{   t.trt<-table(trt)
    contrast.matrix<-NULL
    for(i in 1:(length(t.trt)-1))
    {   temp.contrast<-rep(0,length(trt))
        temp.contrast[trt==names(t.trt)[i]]<- 1
        temp.contrast[trt==names(t.trt)[length(t.trt)]]<- -1
        contrast.matrix<-cbind(contrast.matrix,temp.contrast)
    }
    rownames(contrast.matrix)<-colnames(design)
    colnames(contrast.matrix) <- head(names(t.trt),-1)
    return(contrast.matrix)
}

# "contrast.A" is a contrast matrix from treatment A
# "contrast.B" is a contrast matrix from treatment B
#  makeContrastCross function returns contrast matrix for the interaction between treatment A and treatment B

makeContrastCross<-function(contrast.A,contrast.B,design)
{   a<-ncol(contrast.A)
    b<-ncol(contrast.B)
    contrast.matrix<-NULL
    for(i in 1:a)
      for(j in 1:b)
         contrast.matrix<-cbind(contrast.matrix,contrast.A[,i]*contrast.B[,j])
    rownames(contrast.matrix)<-colnames(design)
    return(contrast.matrix)
}

# contrast for linear trend
#

makeContrastLinear<-function(time)
{   time.l <- unique(time)
    n <- length(time.l)
    temp.list <- sort.list(time.l)
    if(n%%2 ==1)
    {  contrast<-seq(-trunc(n/2),trunc(n/2),by=1)
    } else
    {  contrast<-seq(-(n-1),(n-1),by=2)
    }
    time.contrast<-rep(0,length(time))
    for(i in 1:n)
    { time.contrast[which(time==time.l[temp.list[i]])]<-contrast[i]
    }
    return(time.contrast)
}

# quadratic

makeContrastQuadratic<-function(time)
{   time.l <- unique(time)
    n <- length(time.l)
    temp.list <- sort.list(time.l)
    if(n%%2 ==1)
    {  
       p<-(n-1)/2
       t1<-0
       temp.seq<-NULL
       for(i in 1:p)
       { t1<-t1+sum(seq(from=1,by=2,length=i))
         temp.seq<-c(temp.seq,sum(seq(from=1,by=2,length=i)))
       }
       contrast<-rep(-t1*2/n,n)
       contrast[1:p]<-contrast[1:p]+temp.seq[p:1]
       contrast[(n-p+1):n]<-contrast[(n-p+1):n]+temp.seq[1:p]
       if((t1*2)%%n!=0)
       {  contrast<-contrast*n
       }
    } else
    {  p<-n/2
       t1<-0
       temp.seq<-NULL
       for(i in 1:(p-1))
       { t1<-t1+sum(1:i)
         temp.seq<-c(temp.seq,sum(1:i))
       }
       contrast<-rep(-t1/p,n)
       contrast[1:(p-1)]<-contrast[1:(p-1)]+temp.seq[(p-1):1]
       contrast[(n-p+2):n]<-contrast[(n-p+2):n]+temp.seq[1:(p-1)]
       if(t1%%p!=0)
       {  contrast<-contrast*p
       }
     }    
    time.contrast<-rep(0,length(time))
    for(i in 1:n)
    { time.contrast[which(time==time.l[temp.list[i]])]<-contrast[i]
    }
    return(time.contrast)
}

############## Deprecated, I think ###################

# exists only to factor the cov, cor analysis of sample genotypes (considered private)
.sampleGenotypeApply <- function(func) {
   exps <- exp_designSelection()
   
   assert(length(exps) > 1, "Please select at least two samples")
   assert("genotype" %in% colnames(exps), "Sorry, no genotypes in experiment info.")
   
   genotypes <- exp_designFrame()[exps,"genotype"]

   genotype <- genotypes == genotypes[1]
   results <- apply(exp_dataset()[exps], 1, func, genotype, use = "complete.obs")
   
   exp_showResults(results, deparse(match.call()[[2]]), exps) 
}

MnuCompareCorr_cb<- function(w, u = NULL)
{
    .sampleGenotypeApply(cor)
}

MnuCompareCov_cb<- function(w, u = NULL)
{
    .sampleGenotypeApply(cov)
}
