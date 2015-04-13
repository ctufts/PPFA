rm(list = ls())
library(caret)
library(e1071)
library(randomForest)
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
n.trees <- 5000



methods <- NULL


for( i in 2:ncol(att.importance)){
  print(i)
  
  # identify valid features and create formula
  q <- quantile(att.importance[,i], 0.25)
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
  
  # train svm, gbm, and logistic regression
  svmFit <- ksvm(f, data = train.ds[inTraining, ],
                 prob.model = T ,
                 type = "C-svc")
  
  
  probs.svm <- predict(svmFit, train.ds[-inTraining, ], type = "probabilities")
  
  error.svm <- llfun(train.y[-inTraining,i], probs.svm[, colnames(probs.svm)=="1"])
  
  # gbm model
  set.seed(825)
  gbmFit <- gbm(f, data = train.ds[inTraining, ],
                n.trees = n.trees, 
                distribution = "adaboost")
  # create test set after model
  probs.gbm <- predict(gbmFit, newdata = train.ds[-inTraining, ],
                   n.trees = n.trees, type = "response")
  
  error.gbm <- llfun(train.y[-inTraining,i], probs.gbm)
  
  rfTrain.ds <- cbind(factor(train.y[,i]), train.x[, valid.features])
  names(rfTrain.ds)[1] <- names(train.y)[i]
  
  set.seed(825)
  rfFit <- randomForest(f, data = rfTrain.ds[inTraining, ],
                         n.tree = n.trees
                        ) 
  
  # create test set after model
  probs.rf <- predict(rfFit, newdata = rfTrain.ds[-inTraining, ],
                   type = "prob")
  
  error.rf <- llfun(train.y[-inTraining,i], probs.rf[,2])
  
  # find predictor with smallest error
  # predict on test set and add to result set
  
  model.index <- which.min(c(error.rf, error.svm, error.gbm))
  train.results[(i-1)] <- min(c(error.rf, error.svm, error.gbm))
  
  if(model.index == 1){
    #glm
    print("rf")
    test.probs <- predict(rfFit, newdata = test,
                          type = "prob")[,2]
  }else if(model.index == 2 ){
    #svm
    print("svm")
    test.probs <-predict(svmFit, test,type =  "probabilities")[, 2]
    
  }else{
    print("gbm")
    set.seed(825)
    test.probs <- predict(gbmFit, newdata = test, n.trees = n.trees,
                          type = "response")
  }
  

  
  result.matrix[,i] <- test.probs
  methods <- c(methods, model.index)
  
  
  
}

result.matrix[,1] <- test.x$id
result.df <- as.data.frame(result.matrix)
names(result.df) <- names(submission)

write.csv(result.df, "Data/results/all_submission20150402_v2.csv", quote = F,
          row.names = F)
write.csv(train.results, "Data/results/all_trainResults20150402_v2.csv", quote = F,
          row.names = F)
write.csv(train.results, "Data/results/all_modelList20150402_v2.csv", quote = F,
          row.names = F)

write.csv(result.df, "C:/Users//Chris/Dropbox/Kaggle//ppfa/all_submission20150402_v2.csv", quote = F,
          row.names = F)