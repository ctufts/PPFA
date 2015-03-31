rm(list = ls())
library(FSelector)

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
eliminate.cols <- unique(c(na.cols, constant.cols, 1,2))



# remove all columns with constant values 
train.x <- train.x[,-eliminate.cols]
test.x  <- test.x[,-eliminate.cols]


# only keep columns with least amount of missing data (numeric)
na.counts <- apply(train.x,2, function(x) 100*sum(is.na(x))/length(x))
percentage.real.num <- 1
valid.cols <- which(na.counts < percentage.real.num)

 
train.x <- train.x[,valid.cols]
test.x  <- test.x[,valid.cols]

# only keep categorical columns with more than 1 percent not missing
max.percent.valid <- .1*nrow(train.x)
valid.cat.cols <- which(apply(train.x, 2, 
                        function(x)sum(x != "") > max.percent.valid ))

train.x <- train.x[,valid.cat.cols]
test.x  <- test.x[,valid.cat.cols]


# identify numeric/categorical/ordinal data
numeric_col     <- grep("n_", names(train.x))
categorical_col <- grep("c_", names(train.x))
ordinal_col     <- grep("o_", names(train.x)) 


# impute missing ordinal and numeric values
train.x[, numeric_col][is.na(train.x[,numeric_col])] <- 0
train.x[, ordinal_col][is.na(train.x[,ordinal_col])] <- 0


temp <- as.data.frame(apply(train.y[-1], 2, as.factor))
train.set <- cbind(temp, train.x)


for( i in 1:ncol(temp)){
  print(names(temp)[i])
  f <- formula(paste(names(temp)[i], "~ ."))
  train.set <- cbind(temp[i], train.x)
  names(train.set)[1] <- names(temp)[i]
  att.scores <- random.forest.importance(f, train.set)
  if(i == 1){
    attribute.summary <- att.scores
  }else{
    attribute.summary <- cbind(attribute.summary, att.scores)
  }
}

write.csv(attribute.summary, file = "Data/attributeSummary.csv")
