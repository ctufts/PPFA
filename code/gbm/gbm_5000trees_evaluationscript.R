rm(list = ls())
library(caret)
library(gbm)
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

result.matrix <- matrix(0,nrow = nrow(test.x), ncol = ncol(submission))
p.grid <- expand.grid(
  shrinkage = c(1e-4,1e-3, 1e-2),
  interaction.depth = 1:5
)
train.results <- matrix(0, (ncol(submission)-1), nrow(p.grid))
n.trees <- 5000


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
    train <- createDataPartition(factor(train.y[, i]), p = 0.75)
    
    set.seed(998)
    inTraining <- createDataPartition(train.y[,i], p = .75, list = F)
  #   fitControl <- trainControl(method = "repeatedcv", 
  #                              number = 10, 
  #                              repeats = 1)
    
    train.ds <- cbind(train.y[,i], train.x[, valid.features])
    names(train.ds)[1] <- names(train.y)[i]
    
    print("train model")
    #generate model
    for(j in 1:nrow(p.grid)){
      set.seed(825)
      
      gbmFit <- gbm(f, data = train.ds[inTraining, ],
                       n.trees = n.trees, 
                       interaction.depth = p.grid$interaction.depth[j],
                       shrinkage = p.grid$shrinkage[j],
                       distribution = "adaboost")
      # create test set after model
      probs <- predict(gbmFit, newdata = train.ds[-inTraining, ],
                       n.trees = n.trees, type = "response")
      
      error <- llfun(train.y[-inTraining,i], probs)
      
      train.results[(i-1),j] <- error
    }
  #   test.probs <- predict(gbmFit, newdata = test.x, n.trees = n.trees,
  #                         type = "response")
  #   
  #   result.matrix[,i] <- test.probs
  #   
    
    
  
  }

result.matrix[,1] <- test.x$id
result.df <- as.data.frame(result.matrix)
names(result.df) <- names(submission)


#deterimine best param settings for each classifier
best.settings <- p.grid[apply(train.results, 1, which.min),]

# write.csv(result.df, "Data/results/gbm_5000trees.csv", quote = F,
#           row.names = F)