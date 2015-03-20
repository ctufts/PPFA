rm(list = ls())
library(ggplot2)



train.x <- read.csv("Data/train_values.csv")
train.y <- read.csv("Data/train_labels.csv")
test.x  <- read.csv("Data/test_values.csv")
submission <- read.csv("Data/SubmissionFormat.csv")




# identify constant data
# the below function does not work correctly
# it shoud identify any column which only contains
#na and 0

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




summary(train.x[, categorical_col])
summary(train.x[,ordinal_col])
unique(train.x$o_0317)
