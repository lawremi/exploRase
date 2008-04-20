# Finding patterns in the data (clustering, etc)

# Clustering TODO:
# - Add kmeans (but need way to pick k)
# - Add cutree GUI
# - Visualize using clusterfly, animation

MnuHClust <- function(dist_fun = hopach_discosangle, 
  hclust_fun = function(d) hclust(d, "ward"))
{
  samples <- exp_designSelection()
  ents <- exp_entitySelection()
  
  assert(length(ents) > 1, "Please select more than one entity")
  assert(length(samples) > 1, "Please select more than one condition")
  
  printTask("Hierarchical clustering")
  dataset <- exp_dataset()[,samples,drop=F]
  printOp("Finding distances")
  d <- as.dist(dist_fun(dataset[ents,]))
  addProgress(33)
  printOp("Finding clusters")
  clustering <- hclust_fun(d)
  addProgress(33)
  exp_showClustering(ents, dataset, clustering)
  finishTask()
}

# euclidean distance (similarity by magnitude)
MnuHClust_euclidean_cb <- function(w, u = NULL)
{
  MnuHClust(dist)
}
# euclidean of standardized (mean 0, standard deviation 1) values
# not normally needed
MnuHClust_euclidean_std_cb <- function(w, u = NULL)
{
  MnuHClust(function(d) dist(scale(d)))
}

# correlation distance (similarity by pattern)
MnuHClust_cor_cb <- function(w, u = NULL)
{
  MnuHClust(hopach_discor)
}
# zero correlation distance (similarity by magnitude and pattern)
MnuHClust_zerocor_cb <- function(w, u = NULL)
{
  MnuHClust(hopach_discosangle)
}

# spearman correlation (squared euclidean by ranks)
MnuHClust_spearman_cb <- function(w, u = NULL)
{
  MnuHClust(function(d) dist(apply(d, 1, rank, na.last = "keep")^2))
}

# canberra distance - good for metabolomics (well behaved at zero)
MnuHClust_canberra_cb <- function(w, u = NULL)
{
  MnuHClust(function(d) dist(d, "canberra"))
}

# some distance measures (from the hopach package)

hopach_discosangle<-function(X,na.rm=TRUE){
	X <- as.matrix(X)
	dX<-dim(X)
	p<-dX[1]
	n<-dX[2]
	if(na.rm){
		N<-rowSums(!is.na(X))
		N2<-(!is.na(X))%*%t(!is.na(X))
		X[is.na(X)]<-0
		N<-sqrt(N%*%t(N))/N2
	}
	else
		N<-1
	out<-rowSums(X^2)
	out<-1-N*tcrossprod(X)/sqrt(tcrossprod(out))
	diag(out)<-0
	suppressWarnings(out<-sqrt(out))
	out[out=="NaN"]<-0
	return(out)
}
hopach_discor<-function(X,na.rm=TRUE){
	X <- as.matrix(X)
	p<-dim(X)[1]
	if(na.rm)
		na<-"pairwise.complete.obs"
	else
		na<-"all.obs"
	out<-1-cor(t(X),use=na)
	diag(out)<-0
	suppressWarnings(out<-sqrt(out))
	out[out=="NaN"]<-0
	return(out)
}

findAndShowPatterns <- function(samples, dataset, cutoff)
{
  desc <- paste("Patterns across", paste(samples, collapse=","))
  printTask("Finding patterns")
  printOp("Calculating patterns")
  dataset <- dataset[,samples]
  patterns <- exp_findPatterns(dataset, cutoff)
  addProgress(50)
  exp_showPatterns(patterns, desc, samples)
  finishTask()
}

MnuFindPattern_cb<- function(w, u = NULL)
{
  samples <- exp_designSelection()
  assert(length(samples) > 1, "Please select more than one sample")
  findAndShowPatterns(samples, exp_dataset(), 1/3)
}

findButton_cb <- function(w,info)
{
   radioButtons <- info$buttons
   ent_type <- info$ent_type
   entModel <- getEntityModel(ent_type)
   viewModel <- getEntityView(ent_type)$getModel()
   viewModel$setSortColumnId(match(info$column, colnames(entModel))-1, "decreasing")
   time_slope <- NULL
   active <- matrix(sapply(radioButtons, gtkToggleButtonGetActive),nrow=3)
   for (i in 1:ncol(active)) time_slope[i] <- which(active[,i])
   selected_pattern <- sum(3^(c(1:length(time_slope))-1)*(time_slope-1))
   highlightEntities(entModel[,info$column]==selected_pattern, ent_type, TRUE)
}

updateButton_cb <- function(w, info)
{
  findAndShowPatterns(info$samples, exp_dataset(info$ent_type), 
    info$scale$getValue() / 100)
  info$find_button["sensitive"] <- TRUE
  w["sensitive"] <- FALSE
}

findPatternWindow <- function(timeSet, type, pattern_col, parent = .exp$getMainWindow()) {
    ##--------Main window---------
    patternMain <- gtkWindow("toplevel", show = F)
    patternMain$setTitle("Find Pattern")
    patternMain$setResizable(TRUE)
    #PatternMain$SetUposition(60,140) #gtkWidgetSetUposition
    patternMain$setTransientFor(parent)
    patternMain$setDestroyWithParent(TRUE)
    patternMain$setBorderWidth(5)
    patternMain$setDefaultSize(700,225)

    ##--------Cutoff Slider---------
    cutoffLabel <- gtkLabel("Percent Unchanged")
    cutoffLabel["angle"] <- 90
    cutoffScale <- gtkVScale(min = 0, max = 100, step = 1)
    cutoffScale$setValue(33)
    cutoffScale$setInverted(TRUE)
    gSignalConnect(cutoffScale, "value-changed",
                   function(range)
                   {
                     findButton["sensitive"] <- FALSE
                     updateButton["sensitive"] <- TRUE
                   })
    
    ##--------Layout Table----------
    layoutTable <- gtkTable(7,1+length(timeSet)*2,TRUE)
    layoutTable$setHomogeneous(FALSE)

    ##--------Label packing----------
    timeLabel <- gtkLabel("Time")
    hsep <- gtkHSeparator()
    vsep <- gtkVSeparator()
    arrowBox <- gtkVBox(TRUE,5)
    
    upLabel <- gtkLabel("Up")
    upArrow <- gtkArrow("up")
    upArrow$setSizeRequest(15,15)
    upFixed <- gtkFixed()
    upFixed$put(upLabel,5,17)
    upFixed$put(upArrow,40,17)
        
    downLabel<- gtkLabel("Down")
    downArrow <- gtkArrow("down")
    downArrow$setSizeRequest(15,15)
    downFixed <- gtkFixed()
    downFixed$put(downLabel,5,17)
    downFixed$put(downArrow,40,17)
        
    rightLabel <- gtkLabel("Same")
    rightArrow <- gtkArrow("right")
    rightArrow$setSizeRequest(15,15)
    rightFixed <- gtkFixed()
    rightFixed$put(rightLabel,5,17)
    rightFixed$put(rightArrow,40,17)
        
    arrowBox$packStart(upFixed)
    arrowBox$packStart(rightFixed)
    arrowBox$packStart(downFixed)
        
    if(length(timeSet)*2 >3) 
      buttonWidth <-4
    else buttonWidth <-2

    layoutTable$attach(timeLabel,0,1,0,1)
    #layoutTable$attach(findButton,length(timeSet)*2-buttonWidth,length(timeSet)*2,7,8)
    layoutTable$attach(arrowBox,0,1,2,7)
    layoutTable$attach(hsep,0,1+length(timeSet)*2,1,2)
    layoutTable$attach(vsep,1,2,0,7)

    for (i in 1:length(timeSet))
    {  tLabel <- gtkLabel(timeSet[i])
       layoutTable$attach(tLabel,2*i,2*i+1,0,1)
    }

    ##--------RadioButton Packing && Add Call back------
    #j<-1
    buttons <- NULL
    for (j in 1:10) {
      if(j<length(timeSet))
      {  
          radioButtons <- NULL
          radioBox <- gtkVBox(TRUE,5)
          for (k in 1:3) {
              radioFixed <- gtkFixed()
              radioButton <- gtkRadioButton(radioButtons)
              radioButtons <- c(radioButtons, radioButton)
              #RadioButton$AddCallback("clicked",function(x){Timeslop[j]<<-3})
              #radioButton$setDefaultSize(20,20)
              radioFixed$put(radioButton,7,15)
              radioBox$packStart(radioFixed)
              if (k == 2)
                radioButton$setActive(TRUE)
          }
         layoutTable$attach(radioBox,2*j+1,2*j+2,2,7)
         buttons <- c(buttons, radioButtons)
      }
    }

    findButton <-gtkButton(stock="gtk-find")
    gSignalConnect(findButton, "clicked", findButton_cb, 
      list(buttons = buttons, ent_type = type, column = pattern_col))
    
    updateButton <- gtkButton(stock="gtk-refresh")
    updateButton["sensitive"] <- FALSE
    gSignalConnect(updateButton, "clicked", updateButton_cb, 
      list(scale = cutoffScale, samples = timeSet, ent_type = type,
           find_button = findButton))
    
    buttonBox <- gtkHButtonBox()
    buttonBox$setLayout("edge")
    buttonBox$add(updateButton)
    buttonBox$add(findButton)
    
    hbox <- gtkHBox(FALSE, 5)
    hbox$packStart(cutoffLabel, FALSE, FALSE)
    hbox$packStart(cutoffScale, FALSE, FALSE)
    hbox$packStart(gtkVSeparator(), FALSE, FALSE)
    layoutViewport <- gtkViewport()
    layoutViewport$add(layoutTable)
    layoutScroll <- gtkScrolledWindow()
    layoutScroll$setPolicy("automatic", "automatic")
    layoutScroll$add(layoutViewport)
    hbox$add(layoutScroll)
    
    vbox <- gtkVBox(FALSE, 5)
    vbox$add(hbox)
    vbox$packStart(buttonBox, FALSE, FALSE)
    
    patternMain$add(vbox)
    patternMain
}

# Find Patterns
# Finds patterns in data. Entities within range of 'fraction'
# (centered on median) are considered flat (unchanging). 
# Those below are falling and those above are rising.
# @arguments A data frame of experimental data according to exploRase conventions.
# @arguments The fraction of transitions considered unchanged, centered on median
# Must at least have two columns (to calculate a transition).
# @value a data frame, with a row for each gene. The first column is 
# the sum of \code{x^i} over all \code{i} from 1 to \code{ncol(data)-1}, where
# \code{x} is 1, 2, or 3, depending on whether the pattern is up, same, or down, 
# respectively, for transition \code{i}.
# The second column contains the magnitude of the pattern.
# @keyword arith
exp_findPatterns <-function(data, flat_fraction)
{
   cutoff <- (1 - flat_fraction) / 2
   finalmatrix<-matrix(NA, nrow(data), ncol(data) + 1, byrow=T)
   diffmatrix<-NULL
   for(i in 1:(ncol(data)-1))
      diffmatrix<-cbind(diffmatrix,data[,i+1]-data[,i])
   not_na <- !apply(diffmatrix, 1, function(row) any(is.na(row)))
   pattern<-diffmatrix[not_na,]
   finalmatrix[not_na,] <- 0
   patternmatrix<-matrix(1, dim(pattern)[1], dim(data)[2]-1, byrow=T)
   lowpara<-round(nrow(pattern)*cutoff)
   lowdiv<-apply(pattern,2,sort)[round(nrow(pattern)*cutoff),]
   updiv<-apply(pattern,2,sort)[round(nrow(pattern)*(1-cutoff)),]
   for( j in 1:ncol(pattern))
   {  if ( cutoff == 0) {lowdiv[j]<- 0}
      if ( lowdiv[j] > 0 ) {lowdiv[j] = 0}
      if ( updiv[j] <= 0){updiv[j]<-0}
      if ( updiv[j] > 0){updiv[j]<-updiv[j]}
      if ( cutoff == 0) {updiv[j]<- 0}
      finalmatrix[not_na,2]<-finalmatrix[not_na,2]+abs(pattern[,j])/ncol(pattern)
      patternmatrix[,j]<-(pattern[,j]<lowdiv[j])*3+((pattern[,j]>=lowdiv[j])*
                   (pattern[,j]<=updiv[j]))*2+(pattern[,j]>updiv[j])*1
      finalmatrix[not_na,1]<- (3^(j-1))*(patternmatrix[,j]-1) + finalmatrix[not_na,1]
      finalmatrix[not_na,j+2] <- patternmatrix[,j]
   }
   return(finalmatrix)
}

################### Adding results to the GUI ####################

# Show patterns
# Show the calculated patterns in the GUI
# @arguments a data frame, with a row for each gene and the first column being 
# the pattern codes and the second the magnitude of the pattern 
# (as returned by \code{\link{exp_findPatterns}}).
# @arguments a description of the patterns, for labeling them in the GUI
# @arguments the samples involved in the calculation, for labeling
# @keyword GUI
exp_showPatterns <- 
function(patterns, desc, samples = exp_designSelection()) 
{
  printOp("Showing patterns")
  
  #pattern_mat <- matrix(ncol=length(samples)-1,nrow=nrow(patterns))
  pattern_images <- c("gtk-go-up", "gtk-go-forward", "gtk-go-down")
  
  pattern_mat <- t(apply(patterns[,-c(1,2),drop=F], 1, function(pattern)
    pattern_images[pattern]
  ))
  
  if (any(is.na(pattern_mat)))
    pattern_mat[is.na(pattern_mat)] <- "gtk-question"

  #sapply(1:nrow(patterns), function(i) {
	#  pattern <- patterns[i,1]
	#  sapply(1:ncol(pattern_mat), function(j) {
  #    if (is.na(pattern))
  #      image <- "gtk-dialog-question"
  #    else image <- pattern_images[pattern %% 3 + 1]
	#	  pattern_mat[i, j] <- image
	#	  pattern <- pattern %/% 3
	#  })
  #})
  
  prefix <- paste(".pattern", paste(samples, collapse="."), sep=".")
  patterns <- patterns[,c(1,2)]
  colnames(patterns) <- c(prefix, paste(prefix, "mag", sep="."))
  
  pattern_frm <- as.data.frame(pattern_mat)
  colnames(pattern_frm) <- paste(prefix, 1:ncol(pattern_mat), sep=".")
  
  type <- exp_entityType()
  
  sync <- match(getEntityIds(), rownames(exp_dataset()))
  addInfoColumns(cbind(patterns, pattern_frm)[sync,,drop=F], type, update = F) 
  
  # we update view ourselves
  pattern_col_name <- paste("pattern:", paste(samples, collapse="."))
  view <- getEntityView()
  titles <- sapply(view$getColumns(), gtkTreeViewColumnGetTitle)
  if (!(pattern_col_name %in% titles))
  {
    ncols <- ncol(exp_entityFrame())
    sort_col <- ncols - ncol(pattern_frm)
    
    pattern_col <- gtkTreeViewColumn()
    pattern_col$setTitle(pattern_col_name)
    
    for (i in (ncols - ncol(pattern_mat)):(ncols - 1)) {
      renderer <- gtkCellRendererPixbuf()
      pattern_col$packStart(renderer, TRUE)
      pattern_col$setAttributes(renderer, "stock-id" = i)
    }
    pattern_col$setSizing("fixed")
    pattern_col$setFixedWidth(25 * ncol(pattern_mat))
    view$insertColumn(pattern_col, 2)
    configureViewColumn(pattern_col, sort_col, T, desc, "pattern_finder")
    
    win <- findPatternWindow(samples, type, prefix)
    win$show()
  }
}

# Show a hierarchical clustering
# Shows a hierarchical clustering using the "GGobi" dendrogram viewer.
# @arguments The ids of the entities that were clustered
# @arguments The experimental data that was clustered
# @arguments the clustering, as returned by hclust()
# @keyword GUI
exp_showClustering <- function(ids, data, clustering) {
  printOp("Showing clusters")
  click_callback <- function(w,u=NULL){
    color.click.dn(clustering, ids, data)
    updateColors()
  }
  if (require(cairoDevice))
    Cairo()
  setup.gobidend(clustering, data)
  win <- ggobiTreeWindow(click_callback)
  win$show()
}
