# Remove all previous objects from the data set
rm(list=ls(all=TRUE))

# Load the tidyverse
install.packages('tidyverse')
install.packages('stringr')

# Load e1071 for the SVM (Support Vector Machine') functions
install.packages('e1071')

# Load the library nnet, which will allow us to run multinom, which is a form of logistic regression different than glm.
install.packages('nnet')

# Load cwhmisc and two dependencies.
install.packages('cwhmisc')
install.packages('lattice')
install.packages('grid')
install.packages('rpart')

# Librarys for confustion matrix for confirming svm and lr models
install.packages('caret')
install.packages('heuristica')

# For plotting decision trees
install.packages('rpart.plot')

# This is for testing the accuracy of our logistic regression
install.packages('pscl')

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

data <- Y2015 %>%
  select(starts_with("_"), ends_with("_"))

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

# Convert the binge to 1 and 0
binge$rfbing5[binge$rfbing5 == 1] <- 5
binge$rfbing5[binge$rfbing5 == 2] <- 1
binge$rfbing5[binge$rfbing5 == 5] <- 0

# Create a table to count NAs by column to get a nice list of most problematic columns, which helped me figure out what to take out in the columns above. Don't actually need to run this again, but here in case I need it again.
na_count <-sapply(binge, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
View(na_count)

# Now let's split our "full" data set into a training slice and a testing slice (I know, even though we have a separate test set). 1/5 in testset and 4/5 in trainset.

index <- 1:nrow(binge)
testindex <- sample(index, trunc(length(index)/5))
testset <- binge[testindex,]
trainset <- binge[-testindex,]

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
  table <- table(pred = pred, true=testset$rfbing5)
  # calculate the "score" the degree to which the values in our prediction agree (classAgreement) with values in the test data.  Then add them to a table we create with multiple model predictions.
  df <- rbind(df, data.frame(model=c(name), score=c(classAgreement(table)$diag)))
  return(df)
}

# Get the most frequent baseline, which divides number of false answers by number of total observations to use as a base measurement. This is how well humans did on guessing answers to questions. We need to do better than this with our predictive model. So any model we build needs to do a better job than the humans at getting the right answer.
models.mfc_baseline <- sum(trainset$rfbing5 == 0) / nrow(trainset)
models.results <- data.frame(model=c("MFC"), score=c(models.mfc_baseline))

# First let's check for statistical signficance, and only consider those variables
test_glm<-glm(rfbing5 ~ ., data=trainset)
print(summary(test_glm))

# These are statistically - rfhype5, raceg21,age80, ageg, rfbmi5,smoker3,rfsmok3,drnkwek,rfdrhv5,vegesum, frtlt1, veglt1,rfseat3

# Now let's create a logistic regression model in which we add all variables except for other drinking
models.results <- record_performance("logit", models.results, "all", multinom(rfbing5 ~ ., data=trainset),testset)

# Now let's create a logistic regression model in which we add all variables except for other drinking
models.results <- record_performance("logit", models.results, "all no drnkwek", multinom(rfbing5 ~ . -drnkwek, data=trainset),testset)

# Now let's create a logistic regression model in which we add all variables except for other drinking
models.results <- record_performance("logit", models.results, "all no rfdrhv5", multinom(rfbing5 ~ . -rfdrhv5, data=trainset),testset)

# Now let's create a logistic regression model in which we add all variables except for other drinking
models.results <- record_performance("logit", models.results, "all no drnkwek or rfdrhv5", multinom(rfbing5 ~ . -drnkwek -rfdrhv5 , data=trainset),testset)

# All alex sig
models.results <- record_performance("logit", models.results, "all alex sig", multinom(rfbing5 ~ hcvu651+raceg21+age80+rfbmi5+smoker3+rfsmok3+drnkwek+rfdrhv5+frtlt1+pa300r2+lmtact1+rfseat3, data=trainset),testset)

# All my sig
models.results <- record_performance("logit", models.results, "all my sig", multinom(rfbing5 ~ rfhype5+raceg21+age80+ageg+rfbmi5+smoker3+rfsmok3+drnkwek+rfdrhv5+vegesum+frtlt1+veglt1+rfseat3, data=trainset),testset)


full$scale_score <- scale(full$body_score)
log 

rfbmi (engineer)
frutsum and vegetable sum we can egnineer




# EXPLORATORY ANALYSIS - RASHMI FIGURING OUT

# LOGISTIC REGRESSION MODELS --  SEAN FIGURING OUT

# DECISION TREES -- GRACE FIGURING OUT

# SUPPORT VECTOR MACHINE -- ALEX FIGURING OUT
