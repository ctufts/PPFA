rm(list = ls())
library(caret)
library(gbm)
library(kernlab)
library(randomForest)
att.importance <- read.csv("Data/attributeSummary.csv")
best.params    <- read.csv("Data/gbmParams")
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

# factor level equalization ####
for(i in 1:length(categorical_col)){
  test.x[,categorical_col[i]] <- 
    factor(test.x[, categorical_col[i]],
           levels = levels(as.factor(c(as.character(train.x[,categorical_col[i]]),
                                       as.character(test.x[, categorical_col[i]])
           ))
           )
    )
  train.x[,categorical_col[i]] <- 
    factor(train.x[, categorical_col[i]],
           levels = levels(as.factor(c(as.character(train.x[,categorical_col[i]]),
                                       as.character(test.x[, categorical_col[i]])
           ))
           )
    )
  
}

# initialize result matrix ####
result.matrix <- matrix(0,nrow = nrow(test.x), ncol = ncol(submission))
train.results <- matrix(0, (ncol(submission)-1), 3)
n.trees <- 5000

# perform analysis ####

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
  names(train.ds)[1] <- names(train.y)[i]
  
  print("train model")
  #generate models
  
  # gbm
  set.seed(825)

  gbmFit <- gbm(f, data = train.ds[inTraining, ],
                   n.trees = n.trees, 
                   shrinkage = best.params$shrinkage[(i-1)],
                   interaction.depth = best.params$interaction.depth[(i-1)],
                   distribution = "adaboost")
  
  # create test set after model
  probs.gbm <- predict(gbmFit, newdata = train.ds[-inTraining, ],
                   n.trees = n.trees, type = "response")
  
  error.gbm <- llfun(train.y[-inTraining,i], probs.gbm)
  
  # svm
  train.ds.svm <- cbind(as.factor(train.y[,i]), train.x[, valid.features])
  names(train.ds.svm)[1] <- names(train.y)[i]
  
  set.seed(825)
  
  svmFit <- ksvm(f, data = train.ds.svm[inTraining, ],
                 type = "C-svc", 
                 prob.model = T)
  # create test set after model
  probs.svm <- predict(svmFit, newdata = train.ds.svm[-inTraining, ],
                   type = "probabilities")
  
  error.svm <- llfun(train.y[-inTraining,i], probs.svm[,2])
  
  # random forest
  
  set.seed(825)
  
  rfFit <- randomForest(f, data = train.ds.svm[inTraining, ],
                        n.tree = n.trees)
  # create test set after model
  probs.rf <- predict(rfFit, newdata = train.ds.svm[-inTraining, ],
                       type = "prob")
  
  error.rf <- llfun(train.y[-inTraining,i], probs.rf[,2])
  
  
  
  
  
  
  
  train.results[(i-1), ] <-c(error.gbm, error.svm, error.rf) 
  
  best.model.index <- which.min(c(error.gbm, error.svm, error.rf))
  if(best.model.index == 1){
    print("train model using gbm")
    set.seed(825)
    
    gbmFit <- gbm(f, data = train.ds,
                  n.trees = n.trees, 
                  shrinkage = best.params$shrinkage[(i-1)],
                  interaction.depth = best.params$interaction.depth[(i-1)],
                  distribution = "adaboost")
    
    # create test set after model
    test.probs <- predict(gbmFit, newdata = test.x,
                         n.trees = n.trees, type = "response")
    
    
  }else if(best.model.index == 2){
    print("train model using svm")
    set.seed(825)
    
    svmFit <- ksvm(f, data = train.ds.svm,
                   type = "C-svc", 
                   prob.model = T)
    # create test set after model
    test.probs <- predict(svmFit, newdata = test.x,
                         type = "probabilities")[,2]
  }else{
    print("train model using  rf")
    rfFit <- randomForest(f, data = train.ds.svm,
                          n.tree = n.trees)
    # create test set after model
    test.probs <- predict(rfFit, newdata = test.x,
                        type = "prob")[,2]
  }
  
  
  result.matrix[,i] <- test.probs
  
  
  

}

result.matrix[,1] <- test.x$id
result.df <- as.data.frame(result.matrix)
names(result.df) <- names(submission)
write.csv(result.df, "Data/results/results_3modelComparison.csv", quote = F,
          row.names = F)