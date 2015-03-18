rm(list = ls())
train.x <- read.csv("Data/train_values.csv")
train.y <- read.csv("Data/train_labels.csv")
test.x  <- read.csv("Data//test_values.csv")
# submission.sample <- read.csv("Data//SubmissionFormat.csv")

# characterize the numeric data

numeric_columns <- which(grepl("n_", names(train.x)))
categoric_columns <- which(grepl("c_", names(train.x)))
ordinal_columns <- which(grepl("o_", names(train.x)))


# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
numeric.train <- train.x[, numeric_columns]
numeric.train[is.na(numeric.train)]<- 0
fit <- princomp(numeric.train)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit,  xlabs= rep("", nrow(numeric.train)))



