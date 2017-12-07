# Remove all previous objects from the data set
rm(list=ls(all=TRUE))
options(scipen=999)

# Install/update every package in the known universe or the computer explodes
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
install.packages('party')
install.packages('randomForest')
install.packages('partykit')

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

#Write it out
write.csv(trainset, file="train.csv")
write.csv(testset, file="test.csv")

# Define my evaluation functions
record_performance <- function( model_type, df, name, model, test) {
  # if-else statement to change pred variable depending on model type 
  if (model_type=="svm"){
    pred <- predict(model, test)
  }
  else if (model_type=="tree"){
    pred <- predict(model, test,type="class")
  }
  else if (model_type=="ctree"){
    pred <- predict(model, test)
  }
  else if (model_type=="logit"){
    pred <- predict(model, test,type="class") #we may need to store this as type="response"
  }
  else if (model_type=="ensemble"){
    pred <- predict(model, test) #we may need to store this as type="response"
  }
  # create a table with the prediction from the model and the actual value in the test data
  table <- table(pred = pred, true=testset$rfbing5)
  # calculate the "score" the degree to which the values in our prediction agree (classAgreement) with values in the test data.  Then add them to a table we create with multiple model predictions.
  df <- rbind(df, data.frame(model=c(name), score=c(classAgreement(table)$diag)))
  return(df)
}

# First let's check for statistically signficant variables, and only consider those variables in our model by printing out a coefficient table
test_glm<-glm(rfbing5 ~ ., data=trainset)
print(summary(test_glm))

# This is a GLM with lowest AIC I can get
test_glm <-glm(rfbing5 ~ ageg + educag + smoker3 + rfsmok3 + drnkwek + rfdrhv5 + vegesum + frtlt1 + rfseat3 + drocdy3 + frutda1 +padur1, data=trainset)
print(summary(test_glm))

# These are interesting variables rfhype5, raceg21,age80, ageg, rfbmi5,smoker3,rfsmok3,drnkwek,rfdrhv5,vegesum, frtlt1, veglt1,rfseat3 ageg + educag + smoker3 + rfsmok3 + drnkwek + rfdrhv5 + vegesum + frtlt1 + rfseat3 + drocdy3 + frutda1 +padur1

# Now let's create a logistic regression model in which we add all variables except for other drinking
models.results <- record_performance("logit", models.results, "all", multinom(rfbing5 ~ ., data=trainset),testset)

# Now let's create a logistic regression model in which we add all variables except for other drinking
models.results <- record_performance("logit", models.results, "all no drnkwek", multinom(rfbing5 ~ . -drnkwek, data=trainset),testset)

# Now let's create a logistic regression model in which we add all variables except for other drinking
models.results <- record_performance("logit", models.results, "all no rfdrhv5", multinom(rfbing5 ~ . -rfdrhv5, data=trainset),testset)

# Now let's create a logistic regression model in which we add all variables except for other drinking
models.results <- record_performance("logit", models.results, "all no drnkwek or rfdrhv5", multinom(rfbing5 ~ . -drnkwek -rfdrhv5 , data=trainset),testset)

# Here are all the variables Alex identifeid as significant
models.results <- record_performance("logit", models.results, "all alex sig", multinom(rfbing5 ~ hcvu651+raceg21+age80+rfbmi5+smoker3+rfsmok3+drnkwek+rfdrhv5+frtlt1+pa300r2+lmtact1+rfseat3, data=trainset),testset)

# Here are all the variables Sean identified as signficant
models.results <- record_performance("logit", models.results, "all my sig", multinom(rfbing5 ~ rfbing5 ~ ageg + educag + smoker3 + rfsmok3 + drnkwek + rfdrhv5 + vegesum + frtlt1 + rfseat3 + drocdy3 + frutda1 + padur1, data=trainset),testset)

# Get the most frequent baseline, which divides number of false answers by number of total observations to use as a base measurement. This is how well humans did on guessing answers to questions. We need to do better than this with our predictive model. So any model we build needs to do a better job than the humans at getting the right answer.
models.mfc_baseline <- sum(trainset$rfbing5 == 0) / nrow(trainset)
models.results <- data.frame(model=c("MFC"), score=c(models.mfc_baseline))

# Now let's create a logistic regression model in which we add all variables
models.results <- record_performance("logit", models.results, "logit-all", multinom(rfbing5 ~ . , data=trainset),testset)

# Now let's create a svm regression model in which we add all variables
models.results <- record_performance("svm", models.results, "svm-smoker", svm(rfbing5 ~ smoker3 , data=trainset),testset)

# Now let's create a tree regression model in which we add all variables
models.results <- record_performance("tree", models.results, "tree-smoker", rpart(rfbing5 ~ smoker3 , data=trainset, method="class"),testset)

# Now let's create a tree regression model in which we add all variables
models.results <- record_performance("tree", models.results, "tree-all", rpart(rfbing5 ~ . , data=trainset, method="class"),testset)

# Now let's create a tree regression model in which we add all variables
models.results <- record_performance("tree", models.results, "tree-smoker-80", rpart(rfbing5 ~ smoker3 + age80, data=trainset, method="class"),testset)

# Now let's create a tree regression model in which we add all variables
models.results <- record_performance("tree", models.results, "tree-drnkwek+rfdrhv5", rpart(rfbing5 ~ drnkwek + rfdrhv5, data=trainset, method="class"),testset)

# Now let's create a svm regression model in which we add all variables
models.results <- record_performance("svm", models.results, "svm-all", svm(rfbing5 ~ . , data=trainset),testset)

# Multinomial regression for heavy drinkers
models.results <- record_performance("logit", models.results, "mnHeavy", multinom(rfbing5 ~ rfdrhv5, data=trainset),testset)





tree.mfc_baseline <- sum(test$rfbing5 == 1) / nrow(test)
tree.results <- data.frame(model=c("MFC"), score=c(tree.mfc_baseline))

# Now let's add single variable logsitic regression models to the list. 
tree.results <- record_performance("tree", tree.results, "body_score", rpart(rfbing5 ~ age80, data=train,method="class"),test)


svm.mfc_baseline <- sum(testset$rfbing5 == 0) / nrow(testset)
svm.results <- data.frame(model=c("MFC"), score=c(svm.mfc_baseline))

# Now let's add single variable logsitic regression models to the list. 
svm.results <- record_performance("svm", svm.results, "body_score", svm(rfbing5 ~ age80, data=trainset),testset)


# EXPLORATORY ANALYSIS - RASHMI FIGURING OUT

# LOGISTIC REGRESSION MODELS --  SEAN FIGURING OUT

# DECISION TREES -- GRACE FIGURING OUT
# Traditional Tree =====================================================#

# make a tree model
treeAllModel <-  rpart(rfbing5 ~ . , data=trainset, method="class")

# see how it do
summary(treeAllModel)
# Important variables for treeAllModel: drnkwek + rfdrhv5 + age80+ ageg+ageg5yr+age65yr+hcvu651+chldcnt+chispnc 

# Now let's create a tree regression model in which we add all variables
models.results <- record_performance("tree", models.results, "treeImpVar", rpart(rfbing5 ~ drnkwek + rfdrhv5 + age80+ ageg+ageg5yr+age65yr+hcvu651+chldcnt+chispnc, data=trainset, method="class"),testset)

# Try it without drinking variables- better than baseline, but not by much
models.results <- record_performance("tree", models.results, "treeImpVar_noDrink", rpart(rfbing5 ~ age80+ ageg+ageg5yr+age65yr+hcvu651+chldcnt+chispnc, data=trainset, method="class"),testset)

# plot tree with best fit
fit <- rpart(rfbing5 ~ drnkwek + rfdrhv5 + age80 + ageg+ageg5yr+age65yr+hcvu651+chldcnt+chispnc, method="class", data=trainset)
plot(fit, uniform=TRUE, 
     main="Tree for rfbing5")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "tree.ps", 
     title = "Tree for rfbing5")


# Conditional Inference Tree ===========================================#
library(partykit)
fit2 <- ctree(rfbing5 ~ drnkwek + rfdrhv5 + age80 + chldcnt ,  data=trainset)
plot(fit2, main="rfbing5")
plot(fit2, uniform=TRUE, main="Classification Tree for conditional rfbing5")
models.results <- record_performance("ctree", models.results, "c", ctree(rfbing5 ~ drnkwek + rfdrhv5 + age80 + ageg5yr + chldcnt,  data=trainset),testset)

# create attractive postscript plot of tree 
post(fit2, file = "tree.ps", 
     title = "conditional Tree for rfbing5")

# Random Forest ========================================================#
library(randomForest)
library(partykit)

trainset$rfbing5 <- as.character(trainset$rfbing5)
trainset$rfbing5 <- as.factor(trainset$rfbing5)
fit3 <- randomForest(rfbing5 ~ drnkwek + rfdrhv5 + age80+ ageg+ageg5yr+age65yr+hcvu651+chldcnt+chispnc,   data=trainset)
print(fit3) # view results 
importance(fit3) # importance of each predictor
models.results <- record_performance("tree", models.results, "forest", randomForest(rfbing5 ~ drnkwek + rfdrhv5 + age80+ ageg+ageg5yr+age65yr+hcvu651+chldcnt+chispnc, model='class',  data=trainset),testset)

#=========================================================================================================|
# MOTHAFUCKIN ENSEMBLE TIIIIIIME
#=========================================================================================================|

#ensemble <- data.frame("rfbing5" = testset$rfbing5) 
#ensemble$treeGuess<-predict(fit, testset)
#ensemble$ctreeGuess<-predict(fit2,testset)

  
# Create model values: make all these before adding columns, so things don't get all fucked up
logit<-multinom(rfbing5 ~ . , data=trainset)
  

#Add variable columnns to dataframes for later calculations
    trainset$logitGuess<-predict(logit, trainset)
      testset$logitGuess<-predict(logit, testset)

#Add trees    
    trainset$treeGuess<-predict(fit, trainset)
      testset$treeGuess<-predict(fit, testset)
    trainset$ctreeGuess<-predict(fit2,trainset)
      testset$ctreeGuess<-predict(fit2,testset)

models.results <- record_performance("logit", models.results, "ensemble", multinom(rfbing5 ~ treeGuess+ctreeGuess+logitGuess , data=trainset, type="class"),testset)

