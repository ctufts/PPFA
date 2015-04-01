rm(list = ls())
library(e1071)
library(caret)
library(mlbench)
library(randomForest)
library(doMC)

#initialize two cores
registerDoMC(2)
# error function/metric
llfun <- function(actual, prediction) {
  epsilon <- .000000000000001
  yhat <- pmin(pmax(prediction, epsilon), 1-epsilon)
  logloss <- -mean(actual*log(yhat)
                   + (1-actual)*log(1 - yhat))
  return(logloss)
}

get.binary.columns <- function(x){
  if(lenght(unique(x))==2 & any(is.na(x))){
    return(T)
  }else{
    return(F)
  }
}

train.x <- read.csv("Data/train_values.csv")
train.y <- read.csv("Data/train_labels.csv")
test.x  <- read.csv("Data/test_values.csv")
submission <- read.csv("Data/SubmissionFormat.csv")




# identify constant data
na.zero.cols <- rep(F, ncol(train.x))
for(i in 1:ncol(train.x)){
  na.zero.cols[i] <- all(unique(train.x[,i]) %in% c(0,NA))
}

constant.cols <- which(apply(train.x, 2, function(x)length(unique(x))==1))
eliminate.cols <- unique(c(which(na.zero.cols), constant.cols))


# find all other columns containing a constant and NA (convert to 1/0)

# remove all columns with constant values 
train.x <- train.x[,-eliminate.cols]
test.x  <- test.x[,-eliminate.cols]

# identify numeric/categorical/ordinal data
numeric_col     <- grep("n_", names(train.x))
categorical_col <- grep("c_", names(train.x))
ordinal_col     <- grep("o_", names(train.x)) 

na.vals <- which(is.na(train.x[,ordinal_col]))
train.x[, ordinal_col] <- 0


# categorical data
# for each col of train y get xtabs for all features
for( i in 1:length(categorical_col)){
   
  
  
}
ftable(xtabs( ~ service_a + c_0328 + c_0327, data = cbind(train.x, train.y)))
