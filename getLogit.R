# Version 25 September 2024
# Author: Tommaso Russo
# This function fits a logit model using Landings and Effort data to identify vessels targeting a given species

# Input details:
# Lit is a vector containing Kg of landings for the species
# X is dataframe in wide format containing the effort by vessel/month (rows) in the different cells (colums) 
# thrB is the value of landings (in Kg) to distinguish between accessory (lower than thrB) or target (higher than thrB) landings
# ptrain and ptest define the % or records assigned to training and test datasets, respectively
# lower_value is the minimum value of effort required in a cell
# Allowed methods are logit (binomial) and rf (random forest)

# Output details
# The function returns a list in which:
# confm is the % of records in the diagonal of the confusion matrix comparing predicted and observed values
# CohenK is the value of the Cohen's K coefficient of agreement (assessed using the kappa2 functions of the package irr)
# logit_f is the fitted model (logit or random forest)
# zeroFG is the (optional) names of columns (id of cells) with zero effort or effort below lower_value (excluded in the analysis)


getLogit <- function(Lit, X, thrB, ptrain = 80, ptest = 20, method = "rf", lower_value = 20){
  Litb = 1 * (Lit > thrB)
  XY <- as.data.frame(cbind(X, Litb))
  
  #Partitioning the data
  mext = min(c(length(which(XY$Litb >= 0.5)),
               length(which(XY$Litb < 0.5))))
  
  itrain <- c(sample(which(XY$Litb >= 0.5),
                     floor(mext * ptrain/100), replace = T),
              sample(which(XY$Litb < 0.5), 
                     floor(mext * ptrain/100), replace = T))
  itest <- setdiff(1:nrow(XY), unique(itrain))
  
  FGsum <- apply(XY, 2, sum)
  zeroFG <- numeric(0)
  if(length(FGsum) < 500){
    zeroFG <- colnames(X)[which(FGsum == 0)]
  }else{
    zeroFG <- colnames(X)[which(FGsum < 20)]
  }
  if(length(zeroFG)>0) XY <- XY[,- match(zeroFG, colnames(XY))]
  
  if(method == "binomial"){
    logit_f <- glm(Litb ~. , family = "binomial", data = as.data.frame(XY[itrain,]))
    predf <- 1*(predict(logit_f, newdata = as.data.frame(XY[itest,-ncol(XY)]), type = "response")>0.5)
    confm <- 100*sum(diag(table(predf, Litb[itest])))/sum(table(predf, Litb[itest]))
    cohenK = kappa2(cbind(predf, Litb[itest]))$value
    logit_f <- glm(Litb ~. , family="binomial", data = as.data.frame(XY))
    LogitList <- vector(mode="list", length = 4)
    LogitList[[1]] <- confm
    LogitList[[2]] <- cohenK
    LogitList[[3]] <- logit_f
    LogitList[[4]] <- zeroFG
    names(LogitList) <- c("confm","CohenK","logit_f","zeroFG")
  }
  
  if(method == "rf"){
    CART = rpart(Litb ~., data = as.data.frame(XY[itrain,]))
    predf <- 1*(predict(CART, newdata = as.data.frame(XY[itest,-ncol(XY)]))>0.5)
    confm <- 100*sum(diag(table(predf, Litb[itest])))/sum(table(predf, Litb[itest]))
    cohenK = kappa2(cbind(predf, Litb[itest]))$value
    logit_f <- rpart(Litb ~., data = as.data.frame(XY))
    LogitList <- vector(mode="list", length = 4)
    LogitList[[1]] <- confm
    LogitList[[2]] <- cohenK
    LogitList[[3]] <- logit_f
    LogitList[[4]] <- zeroFG
    names(LogitList) <- c("confm","CohenK","logit_f","zeroFG")
  }
  return(LogitList)
}









