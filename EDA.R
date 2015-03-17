rm(list = ls())
train.x <- read.csv("Data/train_values.csv")
train.y <- read.csv("Data/train_labels.csv")

# encode the outcomes
possible.outcomes <- c(0,1)
unique.y <- expand.grid(possible.outcomes, possible.outcomes, possible.outcomes,
            possible.outcomes, possible.outcomes, possible.outcomes,
            possible.outcomes, possible.outcomes, possible.outcomes,
            possible.outcomes, possible.outcomes, possible.outcomes,
            possible.outcomes, possible.outcomes
            )

# characterize the numeric data
# - how much data is missing for each column
# - which are integer, which are floats
