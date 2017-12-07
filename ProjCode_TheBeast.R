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

# Read in the 2015 BRFSS data
Y2015 <- read_csv("data/2015.csv", col_names= TRUE)
View(Y2015)

# Create a subset of the data that only contains data that only contains computed variables by finding columns with underscores at the start https://www.cdc.gov/brfss/annual_data/2015/pdf/codebook15_llcp.pdf. NOTE: Updated this to include calculated variables that end with underscores, too. 
data <- Y2015 %>%
  select(starts_with("_"), ends_with("_"))

# Then rename the columns to get rid of the stupid underscores that are screwing stuff up and make them lowercase.
names(data) <- names(data) %>%
  tolower() %>%
  str_replace_all("_", "")
View(data)

# Read in FIPS Code data set to match state codes with state names
fips<-read_csv("data/fipscodes.csv", col_types = cols(STATE_FIPS = col_double()))

# Read in 2015 ACS survey data which has selected economic characteristics as percentages like health coverage, unemployment, poverty, food stamps, people making less than 10K, people making more than 200K
census <- read_csv("data/census.csv")

# Join fips code with master BRFSS data, to get state names, then join to the census data 
data <- left_join(data, fips, by = c("state" = "STATE_FIPS"))  
data <- left_join(data, census, by = c("STATE_NAME" = "state_name"))  

data <- data %>%
  select(-c(STUSAB,STATE_NAME,STATENS,state.y))
    
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

# Let's do some feature engineering on the handful of variables that are non categorical in our data set.

#frutsum- base 10
binge$log10frutsum<-log10(binge$frutsum)

#vegsum- base 10
binge$log10frutsum<-log10(binge$vegesum)

#frutsum- base 2
binge$log2frutsum<-log2(binge$frutsum)

#vegsum- base 2
binge$log2frutsum<-log2(binge$vegesum)

#do you even food pyramid bro
binge$highVeg<- (binge$log2frutsum * binge$log2frutsum)

#mixed drinks
binge$mixedDrinks <- scale(binge$drnkwek*binge$rfdrhv5)

# Take the log to normalize all of our census variables
binge$log2unemployment <- log2(binge$unemployment)
binge$log2less_10K <- log2(binge$less_10K)
binge$log2more_200k <- log2(binge$more_200k)
binge$log2foodstamp <- log2(binge$foodstamp)
binge$log2healthinsurance <- log2(binge$healthinsurance)
binge$log2poverty <- log2(binge$poverty)

# Create an inequality index by dividing more than 200K percentage by less than 10K percentage,with higher number meaning more unequal. 
binge <- binge %>%
  mutate(inequality = more_200k/less_10K)

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
test_glm <-glm(rfbing5 ~ rfhype5 + asthms1 +  ageg + rfbmi5 + educag + smoker3 + rfsmok3 + drnkwek + rfdrhv5 + frutsum + frtlt1 + veg23 + minac11 + pastrng + rfseat2 + rfseat3 + drocdy3 + ftjuda1 + padur1 + padur2 + foodstamp, data=trainset)
print(summary(test_glm))

# Get the most frequent baseline, tells us how many people are not binge drinkers. Any model we build needs to do better than this baseline.
models.mfc_baseline <- sum(trainset$rfbing5 == 0) / nrow(trainset)
models.results <- data.frame(model=c("MFC"), score=c(models.mfc_baseline))

# Now let's create a logistic regression model in which we add all variables 
models.results <- record_performance("logit", models.results, "all", multinom(rfbing5 ~ ., data=trainset),testset)

# Now let's create a logistic regression model in which we add all variables except for drnkwek
models.results <- record_performance("logit", models.results, "all no drnkwek", multinom(rfbing5 ~ . -drnkwek, data=trainset),testset)

# Now let's create a logistic regression model in which we add all variables except for rfdrhv5
models.results <- record_performance("logit", models.results, "all no rfdrhv5", multinom(rfbing5 ~ . -rfdrhv5, data=trainset),testset)

# Now let's create a logistic regression model in which we add all variables except for no drnkwek or rfdrhv5
models.results <- record_performance("logit", models.results, "all no drnkwek or rfdrhv5", multinom(rfbing5 ~ . -drnkwek -rfdrhv5 , data=trainset),testset)

# Here is all of our significant variables in our earlier glm
models.results <- record_performance("logit", models.results, "all sig variables", multinom(rfbing5 ~ rfhype5 + asthms1 +  ageg + rfbmi5 + educag + smoker3 + rfsmok3 + drnkwek + rfdrhv5 + frutsum + frtlt1 + veg23 + minac11 + pastrng + rfseat2 + rfseat3 + drocdy3 + ftjuda1 + padur1 + padur2 + foodstamp, data=trainset),testset)

# Here is all of our significant variables in earlier glm except drnkwek and rfdrhv5
# THIS IS OUR BEST MODEL
models.results <- record_performance("logit", models.results, "all sig variables except drinking", multinom(rfbing5 ~ rfhype5 + asthms1 +  ageg + rfbmi5 + educag + smoker3 + rfsmok3 + frutsum + frtlt1 + veg23 + minac11 + pastrng + rfseat2 + rfseat3 + drocdy3 + ftjuda1 + padur1 + padur2 + foodstamp, data=trainset),testset)

#### Now let's test SVM Models
# Now let's create a svm regression model in which we add all variables
# Set the baseline
svmmodels.mfc_baseline <- sum(trainset$rfbing5 == 0) / nrow(trainset)
svmmodels.results <- data.frame(model=c("MFC"), score=c(svmmodels.mfc_baseline))

svmmodels.results <- record_performance("svm", svmmodels.results, "svm all sig var", svm(rfbing5 ~ rfhype5 + asthms1 +  ageg + rfbmi5 + educag + smoker3 + rfsmok3 + frutsum + frtlt1 + veg23 + minac11 + pastrng + rfseat2 + rfseat3 + drocdy3 + ftjuda1 + padur1 + padur2 + foodstamp, data=trainset),testset)
svmmodels.results <- record_performance("svm", svmmodels.results, "svm all", svm(rfbing5 ~ ., data=trainset),testset)



### Now let's test Decision Tree Models

# Create a decision tree accounting for all variables
treeAllModel <-  rpart(rfbing5 ~ . , data=trainset, method="class")
# Print out a summary
summary(treeAllModel)

# Important variables for treeAllModel: drnkwek + mixedDrinks + rfdrhv5 + fc60 + maxvo2 + age80+ ageg+ageg5yr+age65yr 

# Set baseline for tree model
treemodels.mfc_baseline <- sum(trainset$rfbing5 == 0) / nrow(trainset)
treemodels.results <- data.frame(model=c("MFC"), score=c(treemodels.mfc_baseline))

# Now let's create a tree regression model in which we add all variables
treemodels.results <- record_performance("tree", treemodels.results, "treeallVar", rpart(rfbing5 ~ ., data=trainset, method="class"),testset)

# Now let's create a tree regression model in which we add variables important in prior runs
treemodels.results <- record_performance("tree", treemodels.results, "treeOldImpVar", rpart(rfbing5 ~ drnkwek + rfdrhv5 + age80+ ageg+ageg5yr+age65yr+hcvu651+chldcnt+chispnc, data=trainset, method="class"),testset)

# Now let's create a tree regression model in which we add variables important in prior runs
treemodels.results <- record_performance("tree", treemodels.results, "treeNewImpVar", rpart(rfbing5 ~ drnkwek + mixedDrinks + rfdrhv5 + fc60 + maxvo2 + age80+ ageg+ageg5yr+age65yr , data=trainset, method="class"),testset)

# Try it without drinking variables using old variables better than baseline, but not by much
treemodels.results <- record_performance("tree", treemodels.results, "treeOldImpVar_noDrink", rpart(rfbing5 ~ age80+ ageg+ageg5yr+age65yr+hcvu651+chldcnt+chispnc, data=trainset, method="class"),testset)

# Now let's create a tree regression model in which we use new imp variables minus drinking variables
treemodels.results <- record_performance("tree", treemodels.results, "treeNewImpVar_noDrink", rpart(rfbing5 ~ fc60 + maxvo2 + age80+ ageg+ageg5yr+age65yr , data=trainset, method="class"),testset)

# plot tree with best fit
fit <- rpart(rfbing5 ~ drnkwek + rfdrhv5 + age80 + ageg+ageg5yr+age65yr+hcvu651+chldcnt+chispnc, method="class", data=trainset)
plot(fit, uniform=TRUE, 
     main="Tree for rfbing5")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "tree.ps", 
     title = "Tree for rfbing5")


# Conditional Inference Tree (Doesn't add anything, so leave out of ensemble#
library(partykit)
fit2 <- ctree(rfbing5 ~ drnkwek + rfdrhv5 + age80 + chldcnt ,  data=trainset)
plot(fit2, main="rfbing5")
plot(fit2, uniform=TRUE, main="Classification Tree for conditional rfbing5")
treemodels.results <- record_performance("ctree", treemodels.results, "c", ctree(rfbing5 ~ drnkwek + rfdrhv5 + age80 + ageg5yr + chldcnt,  data=trainset),testset)

# create attractive postscript plot of tree 
post(fit2, file = "tree.ps", 
     title = "conditional Tree for rfbing5")

# Let's run several Random Forests ==================#
# Load randomForest library
library(randomForest)

trainset$rfbing5 <- as.character(trainset$rfbing5)
trainset$rfbing5 <- as.factor(trainset$rfbing5)

fit3 <- randomForest(rfbing5 ~ drnkwek + rfdrhv5 + age80+ ageg+ageg5yr+age65yr+hcvu651+chldcnt+chispnc,   data=trainset)
print(fit3) # view results 
importance(fit3) # importance of each predictor

treemodels.results <- record_performance("tree", treemodels.results, "forest_oldvar", randomForest(rfbing5 ~ drnkwek + rfdrhv5 + age80+ ageg+ageg5yr+age65yr+hcvu651+chldcnt+chispnc, model='class',  data=trainset),testset)


treemodels.results <- record_performance("tree", treemodels.results, "forest_oldvar", randomForest(rfbing5 ~ drnkwek + rfdrhv5 + age80+ ageg+ageg5yr+age65yr+hcvu651+chldcnt+chispnc, model='class',  data=trainset),testset)

treemodels.results <- record_performance("tree", treemodels.results, "forest_old_nodrink", randomForest(rfbing5 ~ age80+ ageg+ageg5yr+age65yr+hcvu651+chldcnt+chispnc, model='class',  data=trainset),testset)

treemodels.results <- record_performance("tree", treemodels.results, "forest_newvar", randomForest(rfbing5 ~ drnkwek + mixedDrinks + rfdrhv5 + fc60 + maxvo2 + age80+ ageg+ageg5yr+age65yr, model='class',  data=trainset),testset)



#=========================================================================================================|
# Make and ensemble model out of best random forest and best logistic regression
#=========================================================================================================|
# Redefine our best logistic regression model as logit
logit<-multinom(rfbing5 ~ . , data=trainset)

# Redefine our desicion tree best model as "fit"

# Now let's create a tree regression model in which we add variables important in prior runs

fit <-rpart(rfbing5 ~ drnkwek + rfdrhv5 + age80+ ageg+ageg5yr+age65yr+hcvu651+chldcnt+chispnc, data=trainset, method="class")


# Redefine our random forest best model as "fit2"
fit2<- randomForest(rfbing5 ~ drnkwek + rfdrhv5 + age80+ ageg+ageg5yr+age65yr+hcvu651+chldcnt+chispnc, model='class',  data=trainset)



#Add variable columnns to dataframes for later calculations, with predictions from our best logistic regression model
trainset$logitGuess<-predict(logit, trainset)
testset$logitGuess<-predict(logit, testset)
      
#Add variable columnns to dataframes for later calculations, with predictions from our best decision tree model
trainset$treeGuess<-predict(fit, trainset)
testset$treeGuess<-predict(fit, testset)

#Add variable columnns to dataframes for later calculations, with predictions from our best decision random forest model
trainset$forestGuess<-predict(fit2,trainset)
testset$forestGuess<-predict(fit2,testset)

# Run the ensemble model
newmodels.results <- record_performance("logit", newmodels.results, "ensemble", multinom(rfbing5 ~ treeGuess+forestGuess+logitGuess , data=trainset, type="class"),testset)

newmodels.results <- record_performance("logit", newmodels.results, "ensemble", multinom(rfbing5 ~ treeGuess+forestGuess , data=trainset, type="class"),testset)

