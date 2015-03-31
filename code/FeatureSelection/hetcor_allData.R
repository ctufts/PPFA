rm(list = ls())
library(e1071)
library(caret)
library(polycor)

# error function/metric
llfun <- function(actual, prediction) {
  epsilon <- .000000000000001
  yhat <- pmin(pmax(prediction, epsilon), 1-epsilon)
  logloss <- -mean(actual*log(yhat)
                   + (1-actual)*log(1 - yhat))
  return(logloss)
}

train.x <- read.csv("Data/train_values.csv")
train.y <- read.csv("Data/train_labels.csv")
test.x  <- read.csv("Data/test_values.csv")
submission <- read.csv("Data/SubmissionFormat.csv")




# identify constant data
na.cols       <- which(apply(train.x, 2, function(x)all(is.na(x))))
constant.cols <- which(apply(train.x, 2, function(x)length(unique(x))==1))
eliminate.cols <- unique(c(na.cols, constant.cols))



# remove all columns with constant values 
train.x <- train.x[,-eliminate.cols]
test.x  <- test.x[,-eliminate.cols]


# only keep columns with least amount of missing data
na.counts <- apply(train.x,2, function(x) 100*sum(is.na(x))/length(x))
percentage.real.num <- 1
valid.cols <- which(na.counts < percentage.real.num)

# 
train.x <- train.x[,valid.cols]
test.x  <- test.x[,valid.cols]

# identify numeric/categorical/ordinal data
numeric_col     <- grep("n_", names(train.x))
categorical_col <- grep("c_", names(train.x))
ordinal_col     <- grep("o_", names(train.x)) 


# impute missing ordinal and numeric values
train.x[, numeric_col][is.na(train.x[,numeric_col])] <- 0
train.x[, ordinal_col][is.na(train.x[,ordinal_col])] <- 0

temp <- as.data.frame(apply(train.y[-1], 2, as.factor))
c <- hetcor( cbind(temp,train.x), std.err = F)


