# Remove all previous objects from the data set
rm(list=ls(all=TRUE))

# Load the tidyverse
library(tidyverse)
library(stringr)

# Load e1071 for the SVM (Support Vector Machine) functions
library(e1071)

# Load the library nnet, which will allow us to run multinom, which is a form of logistic regression different than glm.
library(nnet)

# Load cwhmisc and two dependencies.
library(cwhmisc)
library(lattice)
library(grid)
library(rpart)

# Librarys for confustion matrix for confirming svm and lr models
library(caret)
library(heuristica)

# For plotting decision trees
library(rpart.plot)

# This is for testing the accuracy of our logistic regression
library(pscl)
# Read in the 2015 data
Y2015 <- read_csv("data/2015.csv", col_names= TRUE)
View(Y2015)

# Create a subset of the data that only contains data that only contains computed variables by finding columns with underscores at the start https://www.cdc.gov/brfss/annual_data/2015/pdf/codebook15_llcp.pdf

data <- Y2015 %>%
  select(starts_with("_"))

# Then rename the columns to get rid of the stupid underscores that are screwing stuff up and make them lowercase.
names(data) <- names(data) %>%
  tolower() %>%
  str_replace_all("_", "")
View(data)

# First, filter only rows that contain binge drinking 1 (no), 2 (yes) (removing the 9 and other values). Then remove five columns that have lots of NA values to get a workable dataset. And then omit any rows with NA values.
binge <- data %>%
  filter(rfbing5 == c(1,2)) %>%
  select(-cllcpwt,-crace1,-cprace,-flshot6,-pneumo2) %>%
  na.omit()

# Create a table to count NAs by column to get a nice list of most problematic columns, which helped me figure out what to take out in the columns above. Don't actually need to run this again, but here in case I need it again.
na_count <-sapply(binge, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
View(na_count)

# Now let's split our data into a test set and train set
# Set random seed. Don't remove this line.
set.seed(1)

# Shuffle the dataset, call the result shuffled
n <- nrow(binge)
shuffled <- binge[sample(n),]

# Split the data in train and test
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]

# Define my evaluation functions
record_performance <- function( model_type, df, name, model, test) {
  # if-else statement to change pred variable depending on model type
  if (model_type=="svm"){
    pred <- predict(model, test)
  }
  else if (model_type=="tree"){
    pred <- predict(model, test,type="class")
  }
  else if (model_type=="logit"){
    pred <- predict(model, test,type="class") #we may need to store this as type="response"
  }
  # create a table with the prediction from the model and the actual value in the test data
  table <- table(pred = pred, true=test$rfbing5)
  # calculate the "score" the degree to which the values in our prediction agree (classAgreement) with values in the test data.  Then add them to a table we create with multiple model predictions.
  df <- rbind(df, data.frame(model=c(name), score=c(classAgreement(table)$diag)))
  return(df)
}

# Get the most frequent baseline, which divides number of false answers by number of total observations to use as a base measurement. This is how well humans did on guessing answers to questions. We need to do better than this with our predictive model. So any model we build needs to do a better job than the humans at getting the right answer.
lr.mfc_baseline <- sum(test$rfbing5 == 2) / nrow(test)
lr.results <- data.frame(model=c("MFC"), score=c(lr.mfc_baseline))

# Now let's add single variable logsitic regression models to the list.
lr.results <- record_performance("logit", lr.results, "frtresp", multinom(rfbing5 ~ frtresp, data=train),test)

# EXPLORATORY ANALYSIS - RASHMI FIGURING OUT

# LOGISTIC REGRESSION MODELS --  SEAN FIGURING OUT

# DECISION TREES -- GRACE FIGURING OUT

# SUPPORT VECTOR MACHINE -- ALEX FIGURING OUT
