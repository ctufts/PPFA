rm(list = ls())
library(e1071)
library(caret)

llfun <- function(actual, prediction) {
  epsilon <- .000000000000001
  yhat <- pmin(pmax(prediction, epsilon), 1-epsilon)
  logloss <- -mean(actual*log(yhat)
                   + (1-actual)*log(1 - yhat))
  return(logloss)
}

train.x <- read.csv("Data/train_values.csv")
train.y <- read.csv("Data/train_labels.csv")
test.x  <- read.csv("Data//test_values.csv")
# submission.sample <- read.csv("Data//SubmissionFormat.csv")

# find all columns with constant or all NA values
same.info <- which(apply(train.x, 2, function(x)length(unique(x))==1))
no.info <- which(apply(train.x, 2, function(x)all(is.na(x))))
eliminate.cols <- unique(c(same.info, no.info))

# remove columns
train.x <- train.x[,-eliminate.cols]
test.x  <- test.x[,-eliminate.cols]
# characterize the numeric data

numeric_columns <- which(grepl("n_", names(train.x)))
categoric_columns <- which(grepl("c_", names(train.x)))
ordinal_columns <- which(grepl("o_", names(train.x)))

training.index <- sample(1:nrow(train.y), .8*nrow(train.y))
train <- train.x[training.index,numeric_columns]
test  <- train.x[-training.index,numeric_columns]

output.cols <- grep("service",names(train.y))

results <- rep(0, length(output.cols))
for(i in 1:length(output.cols)){
  print(i)
  m <- naiveBayes(x = train, y = factor(train.y[training.index,output.cols[i]]))
  p <- predict(m, newdata = test, type = "raw")
  p[is.na(p)] <- 0
  results[i] <- llfun(train.y[-training.index, output.cols[i]],p[,2])
  
}