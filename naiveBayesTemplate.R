rm(list = ls())
library(e1071)
library(caret)

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

# identify numeric/categorical/ordinal data
numeric_col     <- grep("n_", names(train.x))
categorical_col <- grep("c_", names(train.x))
ordinal_col     <- grep("o_", names(train.x)) 

results <- matrix(0, nrow = nrow(test.x), ncol = (ncol(train.y)-1))


# create test set after model
# probs <- predict(glmFit1, newdata = train.ds[-inTraining, ])
ll <- rep(0, (ncol(train.y)-1))
set.seed(998)
for(i in 2:ncol(train.y)){
  print(i)
  # create partition
  
  inTraining <- createDataPartition(train.y[,i] , p = .8, list = F)

  #generate model
  
  m <- naiveBayes(x = train.x[inTraining, numeric_col], 
                  y = train.y[inTraining,i] )
  p <- predict( m, train.x[-inTraining, numeric_col], type = "raw")
  p[is.na(p)] <- 0.5
#   results[,i] <- p[,2]
  ll[(i-1)] <- llfun(train.y[-inTraining, i],p[,2])
}



