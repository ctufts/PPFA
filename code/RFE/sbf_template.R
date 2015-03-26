rm(list = ls())
library(e1071)
library(caret)
library(mlbench)
library(randomForest)

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


# ensure the results are repeatable
set.seed(7)
# define the control using a random forest selection function
control <- sbfControl(functions=rfSBF, method="cv", number=10)
# run the RFE algorithm
results <- sbf(train.x[, categorical_col], train.y[,2],sbfControl = control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
