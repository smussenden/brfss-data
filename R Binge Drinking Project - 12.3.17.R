library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)


#load the training file
Data.Binge <- read.csv("BingeTrainData.csv", stringsAsFactors = F,header=T)
Data.Binge<-Data.Binge[-1]

Data.Binge$rfbing5<- as.factor(Data.Binge$rfbing5)

train.indices <- which(sample.split(Data.Binge$rfbing5,SplitRatio = 0.70))
sampledata= Data.Binge[train.indices, ]

#model_1 identifies the significant variables (most asteriks = most significant = best p value = less than .05 (between 0 and .05)
model_1 = glm(rfbing5 ~ ., data =sampledata, family = "binomial")
summary(model_1)

#model 2 trim down significant variables; AIC estimates the quality of each model
model_2<- stepAIC(model_1, direction="both")
summary(model_2)

#vif() is optional that will help in early models to remove items ,normally vif values greater than 4 is really bad
vif(model_2)

#model 3 focus on the significant variables
model_3<-glm(formula = rfbing5 ~ state + strwt + rawrake + wt2rake + rfhlth + 
               hcvu651 + casthm1 + asthms1 + prace1 + mrace1 + hispanc + 
               raceg21 + racegr3 + age80 + ageg + rfbmi5 + educag + 
               smoker3 + rfsmok3 + drnkwek + rfdrhv5 + frutsum + vegesum + 
               frtlt1 + veglt1 + pa300r2 + pastrng + lmtact1 + rfseat2 + 
               rfseat3, family = "binomial", data = sampledata)
summary(model_3)

#vif will give a quick idea to remove variables with high vif
vif(model_3)

#with each model, condense variables further to drill down and leave most significant variables remaining with the lowest vif
model_4<-glm(formula = rfbing5 ~ state + strwt + rawrake + wt2rake + rfhlth + 
               hcvu651 + casthm1 + asthms1 + prace1 + mrace1 + hispanc + 
               raceg21 + age80 + ageg + rfbmi5 + educag + 
               smoker3 + rfsmok3 + drnkwek + rfdrhv5 + frutsum + vegesum + 
               frtlt1 + veglt1 + pa300r2 + pastrng + lmtact1 + rfseat2 + 
               rfseat3, family = "binomial", data = sampledata)
summary(model_4)
model_5<-glm(formula = rfbing5 ~ state + strwt + rawrake + wt2rake + rfhlth + 
               hcvu651 + casthm1 + asthms1 + hispanc + 
               raceg21 + age80 + ageg + rfbmi5 + educag + 
               smoker3 + rfsmok3 + drnkwek + rfdrhv5 + frutsum + vegesum + 
               frtlt1 + veglt1 + pa300r2 + pastrng + lmtact1 + rfseat2 + 
               rfseat3, family = "binomial", data = sampledata)
summary(model_5)
model_6<-glm(formula = rfbing5 ~ state + strwt + rawrake + wt2rake + rfhlth + 
               hcvu651 + casthm1 + asthms1 + raceg21 + age80 + ageg + rfbmi5 + 
               smoker3 + rfsmok3 + drnkwek + rfdrhv5 + frutsum + vegesum + 
               frtlt1 + veglt1 + pa300r2 + pastrng + lmtact1 + rfseat2 + 
               rfseat3, family = "binomial", data = sampledata)
summary(model_6)
model_7<-glm(formula = rfbing5 ~ state + strwt + rawrake + wt2rake + rfhlth + 
               hcvu651 + casthm1 + asthms1 + raceg21 + age80 + ageg + rfbmi5 + 
               smoker3 + rfsmok3 + drnkwek + rfdrhv5 + frutsum + vegesum + 
               frtlt1 + veglt1 + pa300r2 + lmtact1 + rfseat2 + 
               rfseat3, family = "binomial", data = sampledata)
summary(model_7)
model_8<-glm(formula = rfbing5 ~ state + strwt + rawrake + wt2rake + rfhlth + 
               hcvu651 + casthm1 + asthms1 + raceg21 + age80 + ageg + rfbmi5 + 
               smoker3 + rfsmok3 + drnkwek + rfdrhv5 + vegesum + 
               frtlt1 + veglt1 + pa300r2 + lmtact1 + rfseat2 + 
               rfseat3, family = "binomial", data = sampledata)
summary(model_8)
model_9<-glm(formula = rfbing5 ~ state + strwt + rawrake + wt2rake + rfhlth + 
               hcvu651 +  asthms1 + raceg21 + age80 + ageg + rfbmi5 + 
               smoker3 + rfsmok3 + drnkwek + rfdrhv5 + vegesum + 
               frtlt1 + veglt1 + pa300r2 + lmtact1 + rfseat2 + 
               rfseat3, family = "binomial", data = sampledata)
summary(model_9)
model_10<-glm(formula = rfbing5 ~ state + rfhlth + 
                hcvu651 +  asthms1 + raceg21 + age80 + ageg + rfbmi5 + 
                smoker3 + rfsmok3 + drnkwek + rfdrhv5 + vegesum + 
                frtlt1 + pa300r2 + lmtact1 + rfseat2 + 
                rfseat3, family = "binomial", data = sampledata)
summary(model_10)
model_11<-glm(formula = rfbing5 ~ state + 
                hcvu651 +  asthms1 + raceg21 + age80 + ageg + rfbmi5 + 
                smoker3 + rfsmok3 + drnkwek + rfdrhv5 + vegesum + 
                frtlt1 + pa300r2 + lmtact1 + rfseat2 + 
                rfseat3, family = "binomial", data = sampledata)
summary(model_11)
model_12<-glm(formula = rfbing5 ~ state + 
                hcvu651 +  asthms1 + raceg21 + age80 + ageg + rfbmi5 + 
                smoker3 + rfsmok3 + drnkwek + rfdrhv5  + 
                frtlt1 + pa300r2 + lmtact1 + rfseat2 + 
                rfseat3, family = "binomial", data = sampledata)
summary(model_12)
model_13<-glm(formula = rfbing5 ~ hcvu651 +  asthms1 + raceg21 + age80 + rfbmi5 + 
                smoker3 + rfsmok3 + drnkwek + rfdrhv5  + 
                frtlt1 + pa300r2 + lmtact1 + rfseat2 + 
                rfseat3, family = "binomial", data = sampledata)
summary(model_13)

model_14<-glm(formula = rfbing5 ~ hcvu651 + raceg21 + age80 + rfbmi5 + 
                smoker3 + rfsmok3 + drnkwek + rfdrhv5  + 
                frtlt1 + pa300r2 + lmtact1 +  rfseat3, family = "binomial", data = sampledata)
summary(model_14)

Final_model<-model_14
library(caret)
library(e1071) 

#Checking with SVM

#data frame/values of the final variables in model 14
my_attributes <- c("rfbing5","hcvu651", "raceg21", "age80","rfbmi5","smoker3","rfsmok3","drnkwek","rfdrhv5","frtlt1","pa300r2","lmtact1","rfseat3")

#saving the final model 14 variables as svm_data
svm_data<- sampledata[my_attributes]

#creates a data frame for svm_data
str(svm_data)

#makes the characters in svm_data all numeric values
svm_data$rfbing5<-as.numeric(as.character(svm_data$rfbing5))

#we are giving control variables used in rfecontrol parameter ,to give how many crossvalidation (CV) steps we need and the method specified as CV is cross validation
control <- rfeControl(functions=caretFuncs, method="cv", number=2)

#svm feature ranking method
results <- rfe(svm_data[,-1], svm_data$rfbing5, sizes=c(1,2), rfeControl=control , method="svmRadial")
print(results)





