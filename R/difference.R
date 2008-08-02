######## Difference measures: Finding outlying or similar entities ##########

MnuDiff_cb<- function(w, u = NULL)
{
   samples <- exp_designSelection()
   
   assert(length(samples) == 2, "Please select two samples")
   
   samples <- sort(samples)
   
   printTask("Calculating difference")
   printOp("Finding difference between", samples[1], "and", samples[2])
   dataset <- exp_dataset()
   reg_y <- dataset[,samples[2]]
   reg_x <- dataset[,samples[1]]
   diff <- exp_calcDiff(reg_x, reg_y)
   addProgress(50)
   
   exp_showResults(diff, "diff", samples)
   finishTask()
}

MnuRegression_cb<- function(w, u = NULL)
{
   samples <- exp_designSelection()
   
   assert(length(samples) == 2, "Please select two samples")
   
   samples <- sort(samples)
   
   printTask("Calculating regression")
   printOp("Fitting regression model between", samples[1], "and", samples[2])
   dataset <- exp_dataset()
   reg_y <- dataset[,samples[2]]
   reg_x <- dataset[,samples[1]]
   lm_residuals <- exp_calcResiduals(reg_x, reg_y)
   addProgress(50)
   
   exp_showResults(lm_residuals, "resid", samples)
   finishTask()
}

MnuAngle_cb<- function(w, u = NULL)
 {
   samples <- exp_designSelection()
   
   assert(length(samples) == 2, "Please select two samples")
   
   samples <- sort(samples)
   
   printTask("Calculating angle")
   printOp("Calculating angle between", samples[1], "and", samples[2])
   dataset <- exp_dataset()
   reg_y <- dataset[,samples[2]]
   reg_x <- dataset[,samples[1]]
   angle_residual <- exp_calcAngleDist(reg_x, reg_y)
   addProgress(50)
   
   exp_showResults(angle_residual, "angle", samples)
   finishTask()
}

MnuMahalanobis_cb<- function(w, u = NULL)
{
   samples <- exp_designSelection()
   
   assert(length(samples) == 2, "Please select two samples")
	
   samples <- sort(samples)
   
   printTask("Calculating Mahalanobis Distance")
   printOp("Calculating Mahalanobis distance between", samples[1], "and", samples[2])
   dataset <- exp_dataset()
   m_dist <- exp_calcMahalanobisDist(dataset[,samples[c(1,2)]])
   addProgress(50)
   
   exp_showResults(m_dist, "maha", samples)
   finishTask()
}

MnuEuclidean_cb<- function(w, u = NULL)
{
   samples <- exp_designSelection()
   ents <- exp_entitySelection()
   assert(length(ents) == 1 && length(samples) > 0, "Please select one entity and at least one sample")
   samples <- sort(samples)
   
   printTask("Calculating Euclidean distance")
   printOp("Calculating Euclidean distance for", ents[1], "across", paste(samples, collapse=", "))
   dataset <- exp_dataset()[,samples,drop=F]
   e_dist <- exp_calcEuclideanDist(dataset, ents[1])
   addProgress(50)
   
   exp_showResults(e_dist, "euc", samples)
   finishTask()
}

MnuCorr_cb<- function(w, u = NULL)
{
   samples <- exp_designSelection()
   ents <- exp_entitySelection()
   
   assert(length(ents) == 1 && length(samples) > 0, "Please select one entity and at least one sample")
   
   samples <- sort(samples)
   printTask("Calculating Correlation distance")
   printOp("Calculating correlation distance for", ents[1], "across", paste(samples, collapse=", "))
   dataset <- exp_dataset()[,samples,drop=F]
   cor_dist <- exp_calcCorrelationDist(dataset, ents)
   addProgress(50)
   
   exp_showResults(cor_dist, "cordist", samples)
   finishTask()
}

MnuCanberra_cb<- function(w, u = NULL)
{
   samples <- exp_designSelection()
   ents <- exp_entitySelection()
   
   assert(length(ents) == 1 && length(samples) > 0, "Please select one entity and at least one sample")
   
   samples <- sort(samples)
   printTask("Calculating Canberra distance")
   printOp("Calculating canberra distance for", ents[1], "across", paste(samples, collapse=", "))
   dataset <- exp_dataset()[,samples,drop=F]
   can_dist <- exp_calcCanberraDist(dataset, ents)
   addProgress(50)
   
   exp_showResults(can_dist, "canberra", samples)
   finishTask()
}

MnuZerocorr_cb<- function(w, u = NULL)
{
   samples <- exp_designSelection()
   ents <- exp_entitySelection()
   
   assert(length(ents) == 1 && length(samples) > 0, "Please select one entity and at least one sample")
   
   samples <- sort(samples)
   
   printTask("Calculating Z-Correlation distance")
   printOp("Calculating zero correlation distance for", ents[1], "across", paste(samples, collapse=", "))
   dataset <- exp_dataset()[,samples,drop=F]
   zero_cor_dist <- exp_calcZeroCorDist(dataset, ents)
   addProgress(50)
   
   exp_showResults(zero_cor_dist, "zcordist", samples)
   finishTask()
}

# Calculate difference
# Just calculates \code{y} - \code{x}. You're probably better off just doing that.
#
# @arguments normally a column from the experimental data matrix
# @arguments normally a column from the experimental data matrix
# @keyword arith
exp_calcDiff <- function(x, y) y - x

# Calculate residuals
# Calculates the residuals from a linear regression of sample \code{y} against sample \code{x}
# @arguments normally a column from the experimental data matrix
# @arguments normally a column from the experimental data matrix
# @keyword regression
exp_calcResiduals <- function(x, y) {
  lm_residuals<-as.matrix(lm(unlist(y)~unlist(x))$residuals)
  var_residual<-sum(lm_residuals*lm_residuals)/(length(lm_residuals)-2)
  lm_residuals/as.numeric(sqrt(var(lm_residuals)))
}

# Calculate angle distance
# Calculates angle distance between sample vectors \code{x} and \code{y}
# @arguments normally a column from the experimental data matrix
# @arguments normally a column from the experimental data matrix
# @keyword arith
exp_calcAngleDist <- function(x, y) atan(log(y)/log(x))-atan(1)

# Calculate Mahalanobis distance
# Calculates Mahalanobis distance between the samples (columns) in the data frame \code{ent_data}
# @arguments a data frame of experimental data, according to exploRase conventions
# @keyword arith
exp_calcMahalanobisDist <- function(ent_data) {
    S<-var(ent_data)
    x<-matrix(rep(1,nrow(ent_data)),ncol=1)%*%t(as.matrix(apply(ent_data,2,mean)))
    Mdist<-NULL
    ranges <- seq(0,nrow(ent_data), by = 1000)
    if (ranges[length(ranges)] != nrow(ent_data))
      ranges <- c(ranges, nrow(ent_data))
    sapply(1:(length(ranges)-1), function(j) {
      ind <- (ranges[j]+1):ranges[j+1]
      Mdist<<-c(Mdist,diag(as.matrix(x[ind,]-ent_data[ind,])%*%solve(S)%*%
                      t(x[ind,]-ent_data[ind,])))
    })
    Mdist
}

# Calculate Euclidean distance
# Calculates Euclidean distance between \code{ent} and the other entities across 
# the samples (columns) in the data frame \code{ent_data}
# @arguments a data frame of experimental data, according to exploRase conventions
# @arguments the id of an entity that is compared to the entities in \code{ent_data}
# @keyword arith
exp_calcEuclideanDist <- function(ent_data, ent) {
    select.x<-ent_data[ent,]
    select.x<-matrix(rep(1,nrow(ent_data)),ncol=1)%*%as.matrix(select.x)
    Edist<-sqrt(rowSums((select.x-ent_data)^2))
    Edist
}

# Calculate correlation distance
# Calculates correlation distance between \code{ent} and the other entities across 
# the samples (columns) in the data frame \code{ent_data}
# @arguments a data frame of experimental data, according to exploRase conventions
# @arguments the id of an entity that is compared to the entities in \code{ent_data}
# @keyword arith
exp_calcCorrelationDist <- function(ent_data, ent) {
    select.x<-ent_data[ent,]
    select.x<-matrix(rep(1,nrow(ent_data)),ncol=1)%*%as.matrix(select.x-mean(as.numeric(select.x)))
    data.cent<-ent_data-as.matrix(apply(ent_data,1,mean))%*%
                                matrix(rep(1,ncol(ent_data)),nrow=1)
    Cordist<-apply(select.x*data.cent,1,sum)/
                      (sqrt(apply(select.x*select.x,1,sum))*
                         sqrt(apply(data.cent*data.cent,1,sum)))
    Cordist
}

# Calculate zero-correlation distance
# Calculates zero (uncentered) correlation distance between \code{ent} and the 
# other entities across the samples (columns) in the data frame \code{ent_data}
# @arguments a data frame of experimental data, according to exploRase conventions
# @arguments the id of an entity that is compared to the entities in \code{ent_data}
# @keyword arith
exp_calcZeroCorDist <- function(ent_data, ent) {
    select.x<-ent_data[ent,]
    select.x<-matrix(rep(1,nrow(ent_data)),ncol=1)%*%as.matrix(select.x)
    Zerocordist<-apply(select.x*ent_data,1,sum)/
                      (sqrt(apply(select.x*select.x,1,sum))*
                         sqrt(apply(ent_data*ent_data,1,sum)))
    Zerocordist
}

# Calculate canberra distance
# Calculates canberra distance between \code{ent} and the 
# other entities across the samples (columns) in the data frame \code{ent_data}
# @arguments a data frame of experimental data, according to exploRase conventions
# @arguments the id of an entity that is compared to the entities in \code{ent_data}
# @keyword arith
exp_calcCanberraDist <- function(ent_data, ent) {
  selected <- as.matrix(ent_data[ent,])
  ent_mat <- as.matrix(ent_data)
  selected_mat <- matrix(rep(selected, nrow(ent_data)), ncol=ncol(ent_data), byrow=T)
  rowSums(abs(ent_mat - selected_mat) / (abs(ent_mat) + abs(selected_mat)))
}
