rm(list = ls())
library(caret)
library(e1071)
att.importance <- read.csv("Data/attributeSummary.csv")

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


# impute numerical and ordinal data to zero
numeric_col     <- grep("n_", names(train.x))
categorical_col <- grep("c_", names(train.x))
ordinal_col     <- grep("o_", names(train.x)) 
# impute missing ordinal and numeric values
train.x[, numeric_col][is.na(train.x[,numeric_col])] <- 0
train.x[, ordinal_col][is.na(train.x[,ordinal_col])] <- 0
test.x[, numeric_col][is.na(test.x[,numeric_col])] <- 0
test.x[, ordinal_col][is.na(test.x[,ordinal_col])] <- 0


for( i in 1:length(categorical_col)){
  l <- unique(c(levels(train.x[, categorical_col[i]]),
                levels(test.x[,categorical_col[i]])
                ))
  test.x[,categorical_col[i]] <- factor(test.x[,categorical_col[i]],
                                        levels = l)
  train.x[,categorical_col[i]] <- factor(train.x[,categorical_col[i]],
                                        levels = l)
  
}
# test.x[, categorical_col][is.na(test.x[,categorical_col])] <- ""
na.train.col <- which(apply(train.x,2, function(x)any(is.na(x))))
na.test.col <- (apply(test.x,2, function(x)sum(is.na(x))))



result.matrix <- matrix(0,nrow = nrow(test.x), ncol = ncol(submission))
train.results <- rep(0, (ncol(submission)-1))







for( i in 2:ncol(att.importance)){
  print(i)
  
  # identify valid features and create formula
  q <- quantile(att.importance[,i], 0.85)
  valid.features <- as.character(att.importance$X[att.importance[,i] > q])
  f <- formula(paste(
    names(train.y)[i], " ~ ",
    paste(valid.features, collapse = " + ")
  ))
  # split training data into training and test sets
 
  
  set.seed(998)
  inTraining <- createDataPartition(train.y[,i], p = .75, list = F)
  
  
  train.ds <- cbind(train.y[,i], train.x[, valid.features])
  test <- test.x[, valid.features]
  names(train.ds)[1] <- names(train.y)[i]
  
  print("train model")
  #generate model
  set.seed(825)
 
  svmFit <- ksvm(f, data = train.ds[inTraining, ],
                prob.model = T ,
                type = "C-svc")
  
  
  probs <- predict(svmFit, train.ds[-inTraining, ], type = "probabilities")
  
  error <- llfun(train.y[-inTraining,i], probs[, colnames(probs)=="1"])
  
  train.results[(i-1)] <- error
  
  test.probs <-predict(svmFit, test,type =  "probabilities")
  
  result.matrix[,i] <- test.probs[, colnames(test.probs)=="1"]
  
  
  
  
}

result.matrix[,1] <- test.x$id
result.df <- as.data.frame(result.matrix)
names(result.df) <- names(submission)

write.csv(result.df, "Data/results/svm_submission20150402.csv", quote = F,
          row.names = F)
write.csv(train.results, "Data/results/svm_trainResults20150402.csv", quote = F,
          row.names = F)
