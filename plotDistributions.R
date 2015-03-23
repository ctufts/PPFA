rm(list = ls())
library(e1071)
library(caret)


gg.histogram <- function(ds, x, x.label, y.label, title){
  
  p <- ggplot(ds, aes_string(x = x)) + geom_histogram() + 
    labs(x=x.label, y=y.label,
         title = "Missing Tags: Number of Days Active") +
    theme(text = element_text(size=16), 
          axis.text = element_text(size=16,color = "black"), 
          panel.grid.major = element_line(colour = "gray"),
          panel.background = element_rect(colour = "black"))
  return(p)
}


# error function/metric
llfun <- function(actual, prediction) {
  epsilon <- .000000000000001
  yhat <- pmin(pmax(prediction, epsilon), 1-epsilon)
  logloss <- -mean(actual*log(yhat)
                   + (1-actual)*log(1 - yhat))
  return(logloss)
}

get.binary.columns <- function(x){
  if(lenght(unique(x))==2 & any(is.na(x))){
    return(T)
  }else{
    return(F)
  }
}

train.x <- read.csv("Data/train_values.csv")
train.y <- read.csv("Data/train_labels.csv")
test.x  <- read.csv("Data/test_values.csv")
submission <- read.csv("Data/SubmissionFormat.csv")




# identify constant data
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



for(i in 1:ncol(train.x)){
  p <- gg.histogram(train.x, names(train.x)[i],
                    x.label = names(train.x)[i],
                    y.label = "count", 
                    title = ""
                    )
  ggsave(paste0("Plots/", names(train.x)[i], ".png"))
}

